/*
 *  NodeImpl.scala
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
package impl

import de.sciss.synth.{ControlABusMap, ControlFillRange, ControlKBusMap, ControlSet}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Ref, TxnExecutor}

object NodeImpl {
  private val EmptyOnEnd: OnEnd = OnEnd(Vector.empty, Vector.empty)

  private final case class OnEnd(direct: Vec[() => Unit], inTxn: Vec[RT => Unit]) {
    def nonEmpty: Boolean = direct.nonEmpty || inTxn.nonEmpty
  }
}

trait NodeImpl extends ResourceImpl with Node {
  import NodeImpl._

  private[this] val onEndFuns = Ref(EmptyOnEnd)

  peer.onEnd {
    val onEnd = onEndFuns.single.swap(EmptyOnEnd)
    if (onEnd.nonEmpty) {
      spawn { implicit itx =>
        implicit val rt: RT = RT.wrap(itx)
        setOnline(value = false)
        processOnEnd(onEnd)
      }
    }
  }

  private[this] def processOnEnd(onEnd: OnEnd)(implicit tx: RT): Unit = {
    onEnd.direct.foreach(_.apply()  )
    onEnd.inTxn .foreach(_.apply(tx))
  }

  // there is still a ScalaCollider actor problem with
  // sending out new messages from an onEnd because that
  // is executed within the osc receiver actor.
  // decouple it instead.
  private def spawn(fun: InTxn => Unit): Unit =
    Executor.defer { TxnExecutor.defaultAtomic(fun) }

  final def onEndTxn(fun: RT => Unit)(implicit tx: RT): Unit =
    onEndFuns.transform(e => e.copy(inTxn = e.inTxn :+ fun))(tx.peer)

  final def onEnd(code: => Unit)(implicit tx: RT): Unit =
    onEndFuns.transform(e => e.copy(direct = e.direct :+ (() => code)))(tx.peer)

  final def read(assoc: (AudioBus, String))(implicit tx: RT): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val reader = BusNodeSetter.reader(name, rb, this)
    registerSetter(reader)
    reader
  }

  final def read(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val reader = BusNodeSetter.reader(name, rb, this)
    registerSetter(reader)
    reader
  }

  /** Associates an audio bus with this node such that the node writes to this bus.
    * This creates a `DynamicAudioBusUser` which will be freed automatically when
    * this node ends.
    */
  final def write(assoc: (AudioBus, String))(implicit tx: RT): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val writer = BusNodeSetter.writer(name, rb, this)
    registerSetter(writer)
    writer
  }

  final def write(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val writer = BusNodeSetter.writer(name, rb, this)
    registerSetter(writer)
    writer
  }

  final def readWrite(assoc: (AudioBus, String))(implicit tx: RT): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val rw = BusNodeSetter.readerWriter(name, rb, this)
    registerSetter(rw)
    rw
  }

  final def readWrite(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val rw = BusNodeSetter.readerWriter(name, rb, this)
    registerSetter(rw)
    rw
  }

  final def map(assoc: (AudioBus, String))(implicit tx: RT): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val mapper = BusNodeSetter.mapper(name, rb, this)
    registerSetter(mapper)
    mapper
  }

  final def map(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val mapper = BusNodeSetter.mapper(name, rb, this)
    registerSetter(mapper)
    mapper
  }

  private def registerSetter(bns: BusNodeSetter)(implicit tx: RT): Unit = {
    requireOnline()
    bns.add()
    onEndTxn {
      implicit tx => bns.remove()
    }
  }

  final def dispose()(implicit tx: RT): Unit = free()

  /** Note: this is graceful in not throwing up if the node was already freed. */
  final def free()(implicit tx: RT): Unit = {
    // requireOnline()
    if (isOnline) {
      tx.addMessage(this, peer.freeMsg)
      setOnline(value = false)
      if (!server.isRealtime) {
        val functions = onEndFuns.swap(EmptyOnEnd)(tx.peer)
        if (functions.nonEmpty) processOnEnd(functions)
      }
    }
  }

  final def set(pairs: ControlSet*)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.setMsg(pairs: _*))
  }

  final def setn(pairs: ControlSet*)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.setnMsg(pairs: _*))
  }

  final def fill(data: ControlFillRange*)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.fillMsg(data: _*))
  }

  final def mapn(pairs: ControlKBusMap*)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.mapnMsg(pairs: _*))
  }

  final def mapan(pairs: ControlABusMap*)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.mapanMsg(pairs: _*)) // , dependencies = this :: Nil /* ?! */)
  }

  final def moveToHead(group: Group)(implicit tx: RT): Unit = {
    require(isOnline && group.isOnline, s"Both source $this and target $group must be online")
    tx.addMessage(this, peer.moveToHeadMsg(group.peer), dependencies = group :: Nil)
  }

  final def moveToTail(group: Group)(implicit tx: RT): Unit = {
    require(isOnline && group.isOnline, s"Both source $this and target $group must be online")
    tx.addMessage(this, peer.moveToTailMsg(group.peer), dependencies = group :: Nil)
  }

  final def moveBefore(target: Node)(implicit tx: RT): Unit = {
    require(isOnline && target.isOnline, s"Both source $this and target $target must be online")
    tx.addMessage(this, peer.moveBeforeMsg(target.peer), dependencies = target :: Nil)
  }

  final def moveAfter(target: Node)(implicit tx: RT): Unit = {
    require(isOnline && target.isOnline, s"Both source $this and target $target must be online")
    tx.addMessage(this, peer.moveAfterMsg(target.peer), dependencies = target :: Nil)
  }

  final def run(state: Boolean)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.runMsg(state), dependencies = Nil)
  }

  final def release(releaseTime: Double)(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.releaseMsg(releaseTime))
  }
}