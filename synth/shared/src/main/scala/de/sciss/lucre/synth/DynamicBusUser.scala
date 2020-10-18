/*
 *  DynamicBusUser.scala
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

import de.sciss.lucre.Disposable

import concurrent.stm.{Ref => ScalaRef}
import de.sciss.synth.{ControlBus => SControlBus, AudioBus => SAudioBus}

trait DynamicUser extends Disposable[RT] {
  /** Adds the user and thereby issues actions on the server. */
  def add    ()(implicit tx: RT): Unit

  /** Removes the user. It is safe to call this method repeatedly,
    * and also if the user has never been added.
    */
  def remove ()(implicit tx: RT): Unit

  /** The user implements `dispose` by simply calling `remove`. */
  def dispose()(implicit tx: RT): Unit = remove()
}

trait DynamicBusUser extends DynamicUser {
  def bus: Bus
}

trait DynamicAudioBusUser extends DynamicBusUser {
  def bus: AudioBus
  def migrateTo(newBus: AudioBus)(implicit tx: RT): DynamicAudioBusUser
}

trait DynamicControlBusUser extends DynamicBusUser {
  def bus: ControlBus
  def migrateTo(newBus: ControlBus)(implicit tx: RT): DynamicControlBusUser
}

object DynamicBusUser {
  def reader(bus: AudioBus): DynamicAudioBusUser =
    new AudioReaderImpl(bus)

  def reader(bus: ControlBus): DynamicControlBusUser =
    new ControlReaderImpl(bus)

  def writer(bus: AudioBus): DynamicAudioBusUser =
    new AudioWriterImpl(bus)

  def writer(bus: ControlBus): DynamicControlBusUser =
    new ControlWriterImpl(bus)

  private abstract class AbstractAudioImpl extends DynamicAudioBusUser with AudioBus.User {
    final val added = ScalaRef(initialValue = false)

    final def busChanged(bus: SAudioBus, isDummy: Boolean)(implicit tx: RT): Unit = ()

    final def migrateTo(newBus: AudioBus)(implicit tx: RT): DynamicAudioBusUser = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: AudioBus): DynamicAudioBusUser
  }

  private final class AudioReaderImpl(val bus: AudioBus) extends AbstractAudioImpl {
    def add   ()(implicit tx: RT): Unit = bus.addReader   (this)
    def remove()(implicit tx: RT): Unit = bus.removeReader(this)

    def newInstance(newBus: AudioBus): DynamicAudioBusUser = reader(newBus)
  }

  private final class AudioWriterImpl(val bus: AudioBus) extends AbstractAudioImpl {
    def add   ()(implicit tx: RT): Unit = bus.addWriter   (this)
    def remove()(implicit tx: RT): Unit = bus.removeWriter(this)

    def newInstance(newBus: AudioBus): DynamicAudioBusUser = writer(newBus)
  }

  private abstract class AbstractControlImpl extends DynamicControlBusUser with ControlBus.User {
    final val added = ScalaRef(initialValue = false)

    final def busChanged(bus: SControlBus)(implicit tx: RT): Unit = ()

    final def migrateTo(newBus: ControlBus)(implicit tx: RT): DynamicControlBusUser = {
      require(newBus.numChannels == bus.numChannels)
      val wasAdded = added.get(tx.peer)
      if (wasAdded) remove()
      val res = newInstance(newBus)
      if (wasAdded) res.add()
      res
    }

    def newInstance(newBus: ControlBus): DynamicControlBusUser
  }

  private final class ControlReaderImpl(val bus: ControlBus) extends AbstractControlImpl {
    def add   ()(implicit tx: RT): Unit = bus.addReader   (this)
    def remove()(implicit tx: RT): Unit = bus.removeReader(this)

    def newInstance(newBus: ControlBus): DynamicControlBusUser = reader(newBus)
  }

  private final class ControlWriterImpl(val bus: ControlBus) extends AbstractControlImpl {
    def add   ()(implicit tx: RT): Unit = bus.addWriter   (this)
    def remove()(implicit tx: RT): Unit = bus.removeWriter(this)

    def newInstance(newBus: ControlBus): DynamicControlBusUser = writer(newBus)
  }
}