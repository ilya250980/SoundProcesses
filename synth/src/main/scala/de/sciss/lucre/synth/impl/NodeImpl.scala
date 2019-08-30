/*
 *  NodeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth.impl

import java.util.concurrent.{Executors, ScheduledExecutorService}

import de.sciss.lucre.synth.{AudioBus, AudioBusNodeSetter, BusNodeSetter, ControlBus, ControlBusNodeSetter, Group, Node, Txn, log}
import de.sciss.synth.{ControlABusMap, ControlFillRange, ControlKBusMap, ControlSet}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{InTxn, Ref, TxnExecutor}

object NodeImpl {
  private val EmptyOnEnd: OnEnd = OnEnd(Vector.empty, Vector.empty)

  private final case class OnEnd(direct: Vec[() => Unit], inTxn: Vec[Txn => Unit]) {
    def nonEmpty: Boolean = direct.nonEmpty || inTxn.nonEmpty
  }

  var poolSize: Option[Int] = None

  lazy val pool: ScheduledExecutorService = {
    // system wide scheduler
    val res = poolSize match {
      case Some(sz) => Executors.newScheduledThreadPool(sz)
      case _        => Executors.newSingleThreadScheduledExecutor()
    }
    sys.addShutdownHook(shutdownScheduler())
    res
  }

  private def shutdownScheduler(): Unit = {
    log("Shutting down scheduler thread pool")
    pool.shutdown()
  }
}

trait NodeImpl extends ResourceImpl with Node {
  import NodeImpl._

  private[this] val onEndFuns = Ref(EmptyOnEnd)

  peer.onEnd {
    val onEnd = onEndFuns.single.swap(EmptyOnEnd)
    if (onEnd.nonEmpty) {
      spawn { implicit itx =>
        implicit val ptx: Txn = Txn.wrap(itx)
        setOnline(value = false)
        processOnEnd(onEnd)
      }
    }
  }

  private[this] def processOnEnd(onEnd: OnEnd)(implicit tx: Txn): Unit = {
    onEnd.direct.foreach(_.apply()  )
    onEnd.inTxn .foreach(_.apply(tx))
  }

  // there is still a ScalaCollider actor problem with
  // sending out new messages from an onEnd because that
  // is executed within the osc receiver actor.
  // decouple it instead.
  private def spawn(fun: InTxn => Unit): Unit =
    pool.submit(new Runnable {
      def run(): Unit = TxnExecutor.defaultAtomic(fun)
    })

  final def onEndTxn(fun: Txn => Unit)(implicit tx: Txn): Unit =
    onEndFuns.transform(e => e.copy(inTxn = e.inTxn :+ fun))(tx.peer)

  final def onEnd(code: => Unit)(implicit tx: Txn): Unit =
    onEndFuns.transform(e => e.copy(direct = e.direct :+ (() => code)))(tx.peer)

  final def read(assoc: (AudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val reader = BusNodeSetter.reader(name, rb, this)
    registerSetter(reader)
    reader
  }

  final def read(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val reader = BusNodeSetter.reader(name, rb, this)
    registerSetter(reader)
    reader
  }

  /** Associates an audio bus with this node such that the node writes to this bus.
    * This creates a `DynamicAudioBusUser` which will be freed automatically when
    * this node ends.
    */
  final def write(assoc: (AudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val writer = BusNodeSetter.writer(name, rb, this)
    registerSetter(writer)
    writer
  }

  final def write(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val writer = BusNodeSetter.writer(name, rb, this)
    registerSetter(writer)
    writer
  }

  final def readWrite(assoc: (AudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val rw = BusNodeSetter.readerWriter(name, rb, this)
    registerSetter(rw)
    rw
  }

  final def readWrite(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val rw = BusNodeSetter.readerWriter(name, rb, this)
    registerSetter(rw)
    rw
  }

  final def map(assoc: (AudioBus, String))(implicit tx: Txn): AudioBusNodeSetter = {
    val (rb, name) = assoc
    val mapper = BusNodeSetter.mapper(name, rb, this)
    registerSetter(mapper)
    mapper
  }

  final def map(assoc: (ControlBus, String))(implicit tx: Txn): ControlBusNodeSetter = {
    val (rb, name) = assoc
    val mapper = BusNodeSetter.mapper(name, rb, this)
    registerSetter(mapper)
    mapper
  }

  private def registerSetter(bns: BusNodeSetter)(implicit tx: Txn): Unit = {
    requireOnline()
    bns.add()
    onEndTxn {
      implicit tx => bns.remove()
    }
  }

  final def dispose()(implicit tx: Txn): Unit = free()

  /** Note: this is graceful in not throwing up if the node was already freed. */
  final def free()(implicit tx: Txn): Unit = {
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

  final def set(pairs: ControlSet*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.setMsg(pairs: _*))
  }

  final def setn(pairs: ControlSet*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.setnMsg(pairs: _*))
  }

  final def fill(data: ControlFillRange*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.fillMsg(data: _*))
  }

  final def mapn(pairs: ControlKBusMap*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.mapnMsg(pairs: _*))
  }

  final def mapan(pairs: ControlABusMap*)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.mapanMsg(pairs: _*)) // , dependencies = this :: Nil /* ?! */)
  }

  final def moveToHead(group: Group)(implicit tx: Txn): Unit = {
    require(isOnline && group.isOnline, s"Both source $this and target $group must be online")
    tx.addMessage(this, peer.moveToHeadMsg(group.peer), dependencies = group :: Nil)
  }

  final def moveToTail(group: Group)(implicit tx: Txn): Unit = {
    require(isOnline && group.isOnline, s"Both source $this and target $group must be online")
    tx.addMessage(this, peer.moveToTailMsg(group.peer), dependencies = group :: Nil)
  }

  final def moveBefore(target: Node)(implicit tx: Txn): Unit = {
    require(isOnline && target.isOnline, s"Both source $this and target $target must be online")
    tx.addMessage(this, peer.moveBeforeMsg(target.peer), dependencies = target :: Nil)
  }

  final def moveAfter(target: Node)(implicit tx: Txn): Unit = {
    require(isOnline && target.isOnline, s"Both source $this and target $target must be online")
    tx.addMessage(this, peer.moveAfterMsg(target.peer), dependencies = target :: Nil)
  }

  final def run(state: Boolean)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.runMsg(state), dependencies = Nil)
  }

  final def release(releaseTime: Double)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.releaseMsg(releaseTime))
  }
}