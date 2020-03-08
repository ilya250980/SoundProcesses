/*
 *  BusNodeSetter.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import de.sciss.synth.{ControlABusMap, AudioBus => SAudioBus, ControlBus => SControlBus}

import scala.concurrent.stm.{Ref => ScalaRef}

trait BusNodeSetter extends DynamicBusUser {
  def node: Node
  def controlName: String
}

trait AudioBusNodeSetter extends BusNodeSetter with DynamicAudioBusUser {
  def migrateTo(newBus: AudioBus)(implicit tx: Txn): AudioBusNodeSetter
}

trait ControlBusNodeSetter extends BusNodeSetter with DynamicControlBusUser {
  def migrateTo(newBus: ControlBus)(implicit tx: Txn): ControlBusNodeSetter
}

/** A factory for setting node controls to read from buses. */
object BusNodeSetter {
  /** Creates a user that sets a control to an audio bus index. It registers a reader
    * with the given bus.
    *
    * @param controlName  the name of the control to set
    * @param bus          the audio-bus from which to read
    * @param node         the node which reads from the bus.
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def reader(controlName: String, bus: AudioBus, node: Node): AudioBusNodeSetter =
    new AudioReaderImpl(controlName, bus, node)

  /** Creates a user that sets a control to a control bus index. It registers a reader
    * with the given bus.
    *
    * @param controlName  the name of the control to set
    * @param bus          the control-bus from which to read
    * @param node         the node which reads from the bus.
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def reader(controlName: String, bus: ControlBus, node: Node): ControlBusNodeSetter =
    new ControlReaderImpl(controlName, bus, node)

  /** Creates a user that sets a control to an audio bus index. It registers a writer
    * with the given bus.
    *
    * @param controlName  the name of the control to set
    * @param bus          the audio-bus to which to write
    * @param node         the node which writes to the bus.
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def writer(controlName: String, bus: AudioBus, node: Node): AudioBusNodeSetter =
    new AudioWriterImpl(controlName, bus, node)

  /** Creates a user that setes a control to a control bus index. It registers a writer
    * with the given bus.
    *
    * @param controlName  the name of the control to set
    * @param bus          the control-bus to which to write
    * @param node         the node which writes to the bus.
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def writer(controlName: String, bus: ControlBus, node: Node): ControlBusNodeSetter =
    new ControlWriterImpl(controlName, bus, node)

  /** Creates a user that sets a control to an audio bus index. It registers both a
    * reader and a writer with the given bus.
    *
    * @param controlName  the name of the control to set
    * @param bus          the audio-bus from which to read and to which to write
    * @param node         the node which reads from and writes to the bus.
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def readerWriter(controlName: String, bus: AudioBus, node: Node): AudioBusNodeSetter =
    new AudioReaderWriterImpl(controlName, bus, node)

  /** Creates a user that sets a control to a control bus index. It registers both a
    * reader and a writer with the given bus.
    *
    * @param controlName  the name of the control to set
    * @param bus          the control-bus from which to read and to which to write
    * @param node         the node which reads from and writes to the bus.
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def readerWriter(controlName: String, bus: ControlBus, node: Node): ControlBusNodeSetter =
    new ControlReaderWriterImpl(controlName, bus, node)

  /** Sets a control be mapped to an audio bus (using `n_mapan`). It registers a
    * reader with the given bus that is freed along with the provided node.
    *
    * @param controlName  the name of the control to be mapped to a bus signal
    * @param bus          the audio-bus from which to read
    * @param node         the node which reads from the control
    *
    * @return the bus user that can be engaged via `add()` and disengaged via `remove()`
    */
  def mapper(controlName: String, bus: AudioBus, node: Node): AudioBusNodeSetter =
    new AudioMapperImpl(controlName, bus, node)

  /** Sets a control be mapped to a control bus (using `n_mapn`). It registers a
    * reader with the given bus that is freed along with the provided node.
    *
    * @param controlName  the name of the control to be mapped to a bus signal
    * @param bus          the control-bus from which to read
    * @param node         the node which reads from the control
    */
  def mapper(controlName: String, bus: ControlBus, node: Node): ControlBusNodeSetter =
    new ControlMapperImpl( controlName, bus, node )

  // provides the `added` state
  private sealed trait ImplLike extends BusNodeSetter {
    final val added = ScalaRef(initialValue = false)
  }

  private sealed trait AudioSetterLike extends ImplLike {
    final def busChanged(b: SAudioBus, isDummy: Boolean)(implicit tx: Txn): Unit =
      if (node.isOnline) node.set(controlName -> b.index)
  }

  private sealed trait ControlSetterLike extends ImplLike {
    final def busChanged(b: SControlBus)(implicit tx: Txn): Unit =
      if (node.isOnline) node.set(controlName -> b.index)
  }

  // implements `busChanged` in terms of a `mapan` command
  private trait AudioMapperLike extends ImplLike {
    final def busChanged(b: SAudioBus, isDummy: Boolean)(implicit tx: Txn): Unit = {
//      val value: ControlABusMap = if (isDummy) controlName -> -1 else controlName -> b
//      node.mapan(true, value)

      if (isDummy) {
        node.mapan(ControlABusMap.Multi(controlName, -1, b.numChannels))
        // node.fill(true, (controlName, b.numChannels, 0f))
      } else {
        node.mapan(controlName -> b)
      }
    }
  }

  private sealed trait ControlMapperLike extends ImplLike {
    final def busChanged(b: SControlBus)(implicit tx: Txn): Unit =
      node.mapn(controlName -> b)
  }

  private abstract class AbstractAudioImpl
    extends ImplLike with AudioBus.User with AudioBusNodeSetter {

    final def migrateTo(newBus: AudioBus)(implicit tx: Txn): AudioBusNodeSetter = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: AudioBus): AudioBusNodeSetter
  }

  private abstract class AbstractControlImpl
    extends ImplLike with ControlBus.User with ControlBusNodeSetter {
    final def migrateTo(newBus: ControlBus)(implicit tx: Txn): ControlBusNodeSetter = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: ControlBus): ControlBusNodeSetter
  }

  private abstract class AbstractAudioReader extends AbstractAudioImpl {
    final def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error(s"Was already added : $this")
      bus.addReader(this)
    }

    final def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) {
        bus.removeReader(this)
        // wasRemoved()
      }
    }

    // protected def wasRemoved()(implicit tx: Txn) = ()
  }

  private abstract class AbstractControlReader extends AbstractControlImpl {
    final def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error(s"Was already added : $this")
      bus.addReader(this)
    }

    final def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeReader(this)
    }
  }

  /*
   *    Careful not use case classes here, as multiple readers / writers for the
   *    same combo might be wanted in a read / write set!
   */
  private final class AudioReaderImpl(val controlName: String, val bus: AudioBus, val node: Node)
    extends AbstractAudioReader with AudioSetterLike {

    def newInstance(newBus: AudioBus): AudioBusNodeSetter =
      reader(controlName, newBus, node)

    override def toString = s"BusNodeSetter.reader($controlName, $bus, $node)"
  }

  private final class ControlReaderImpl(val controlName: String, val bus: ControlBus, val node: Node)
    extends AbstractControlReader with ControlSetterLike {

    def newInstance(newBus: ControlBus): ControlBusNodeSetter =
      reader(controlName, newBus, node)

    override def toString = s"BusNodeSetter.reader($controlName, $bus, $node)"
  }

  private final class AudioMapperImpl(val controlName: String, val bus: AudioBus, val node: Node)
    extends AbstractAudioReader with AudioMapperLike {

    def newInstance(newBus: AudioBus): AudioBusNodeSetter =
      mapper(controlName, newBus, node)

    override def toString = s"BusNodeSetter.mapper($controlName, $bus, $node)"

    //    override protected def wasRemoved()(implicit tx: Txn) = {
    //      println("Yo chuck")
    //    }
  }

  private final class ControlMapperImpl(val controlName: String, val bus: ControlBus, val node: Node)
    extends AbstractControlReader with ControlMapperLike {

    def newInstance(newBus: ControlBus): ControlBusNodeSetter =
      mapper(controlName, newBus, node)

    override def toString = s"BusNodeSetter.mapper($controlName, $bus, $node)"
  }

  /*
   *    Careful not use case classes here, as multiple readers / writers for the
   *    same combo might be wanted in a read / write set!
   */
  private final class AudioWriterImpl(val controlName: String, val bus: AudioBus, val node: Node)
    extends AbstractAudioImpl with AudioSetterLike {

    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error(s"Was already added : $this")
      bus.addWriter(this)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeWriter(this)
    }

    def newInstance(newBus: AudioBus): AudioBusNodeSetter =
      writer(controlName, newBus, node)

    override def toString = s"BusNodeSetter.writer($controlName, $bus, $node)"
  }

  private final class ControlWriterImpl(val controlName: String, val bus: ControlBus, val node: Node)
    extends AbstractControlImpl with ControlSetterLike {

    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error(s"Was already added : $this")
      bus.addWriter(this)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) bus.removeWriter(this)
    }

    def newInstance(newBus: ControlBus): ControlBusNodeSetter =
      writer(controlName, newBus, node)

    override def toString = s"BusNodeSetter.writer($controlName, $bus, $node)"
  }

  /*
   *    Careful not use case classes here, as multiple readers / writers for the
   *    same combo might be wanted in a read / write set!
   */
  private final class AudioReaderWriterImpl( val controlName: String, val bus: AudioBus, val node: Node )
   extends AbstractAudioImpl with AudioSetterLike {

    object dummy extends AudioBus.User {
      def busChanged(b: SAudioBus, isDummy: Boolean)(implicit tx: Txn): Unit = ()
    }

    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error("Was already added : " + this)
      bus.addReader(this)
      bus.addWriter(dummy)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) {
        bus.removeWriter(dummy)
        bus.removeReader(this)
      }
    }

    def newInstance(newBus: AudioBus): AudioBusNodeSetter =
      readerWriter(controlName, newBus, node)

    override def toString = s"BusNodeSetter.readerWriter($controlName, $bus, $node)"
  }

  private final class ControlReaderWriterImpl(val controlName: String, val bus: ControlBus, val node: Node)
    extends AbstractControlImpl with ControlSetterLike {

    object dummy extends ControlBus.User {
      def busChanged(b: SControlBus)(implicit tx: Txn): Unit = ()
    }

    def add()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(true)(tx.peer)
      if (wasAdded) sys.error(s"Was already added : $this")
      bus.addReader(this)
      bus.addWriter(dummy)
    }

    def remove()(implicit tx: Txn): Unit = {
      val wasAdded = added.swap(false)(tx.peer)
      if (wasAdded) {
        bus.removeWriter(dummy)
        bus.removeReader(this)
      }
    }

    def newInstance(newBus: ControlBus): ControlBusNodeSetter =
      readerWriter(controlName, newBus, node)

    override def toString = s"BusNodeSetter.readerWriter($controlName, $bus, $node)"
  }
}