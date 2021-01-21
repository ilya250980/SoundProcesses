/*
 *  MutableRunnerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc.impl

import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.{Disposable, Txn}
import de.sciss.proc.Runner.{Attr, Message, Mutable}
import de.sciss.proc.{Runner, Universe}

import scala.concurrent.stm.Ref

class MutableRunnerImpl[T <: Txn[T]](init: Option[Runner[T]], tx0: T)(implicit val universe: Universe[T])
  extends Mutable[T] with ObservableImpl[T, Runner.State] {

  private[this] val ref         = Ref(init)
  private[this] val refState    = Ref[Runner.State](Runner.Stopped)
  private[this] val refObs      = Ref(init.map(new MutableRef(_, tx0)))

  private def setState(now: Runner.State)(implicit tx: T): Unit = {
    val before = refState.swap(now)
    if (now != before) this.fire(now)
  }

  private final class MutableRef(r: Runner[T], tx0: T) extends Disposable[T] {
    private[this] val obsState    = r         .react { implicit tx => now => setState     (now) } (tx0)
    private[this] val obsMessages = r.messages.react { implicit tx => now => messages.set (now) } (tx0)
    private[this] val obsProgress = r.progress.react { implicit tx => now => progress.set (now) } (tx0)

    override def dispose()(implicit tx: T): Unit = {
      obsState    .dispose()
      obsMessages .dispose()
      obsProgress .dispose()
    }
  }

  override def peer(implicit tx: T): Option[Runner[T]] = ref()

  /** Note that when the peer is updated, the previous peer (if it exists) will be disposed. */
  override def peer_=(value: Option[Runner[T]])(implicit tx: T): Unit = {
    val newObs = value.map(new MutableRef(_, tx))
    refObs.swap(newObs).foreach(_.dispose())
    ref   .swap(value ).foreach(_.dispose())
    // XXX TODO: should we update the new peer to reflect some sort of "target state"?
    value.foreach { r =>
      // XXX TODO: it might be better to pack them into a single IEvent call?
      setState    (r.state)
      messages.set(r.messages.current)
      progress.set(r.progress.current)
    }
  }

  object messages extends Runner.Messages[T] with ObservableImpl[T, List[Message]] {
    private[this] val refValue = Ref(List.empty[Message])

    def set(now: List[Message])(implicit tx: T): Unit = {
      val before = refValue.swap(now)
      if (now != before) fire(now)
    }

    override def current(implicit tx: T): List[Message] =
      ref().fold(refValue())(_.messages.current)
  }

  object progress extends Runner.Progress[T] with ObservableImpl[T, Double] {
    private[this] val refValue = Ref(0.0)

    def set(now: Double)(implicit tx: T): Unit = {
      val before = refValue.swap(now)
      if (now != before) fire(now)
    }

    override def current(implicit tx: T): Double =
      ref().fold(refValue())(_.progress.current)
  }

  override def prepare(attr: Attr[T])(implicit tx: T): Unit =
    ref().foreach(_.prepare(attr))

  override def run()(implicit tx: T): Unit =
    ref().foreach(_.run())

  override def initControl()(implicit tx: T): Unit = () // XXX TODO: are there peers that might need this?

  override def state(implicit tx: T): Runner.State =
    ref().fold(refState())(_.state)

  override def stop()(implicit tx: T): Unit =
    ref().foreach(_.stop())

  override def dispose()(implicit tx: T): Unit = {
    refObs.swap(None).foreach(_.dispose())
    ref   .swap(None).foreach(_.dispose())
  }
}
