/*
 *  ProcRunnerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Runner.Handler

import scala.concurrent.stm.Ref

object ProcRunnerImpl {
  def apply[S <: Sys[S]](obj: Proc[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] = {
    // the transport is simply to get the work done of dynamically creating
    // an aural-obj... a bit of a resource waste?
    val t = h.mkTransport()
    t.addObject(obj)
    new Impl(tx.newHandle(obj), t, h).init(obj)
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Proc[S]],
                                        t: Transport[S],
                                        val handler: Handler[S])
    extends BasicRunnerImpl[S] with ObjViewBase[S, Unit] {

    override def toString = s"Runner.Proc${hashCode().toHexString}"

    def tpe: Obj.Type = Proc

    private[this] val dispatchedState = Ref[Runner.State](Runner.Stopped)
    private[this] val targetState     = Ref[Runner.State](Runner.Stopped)
    private[this] val auralRef        = Ref(Option.empty[AuralObj[S]])
    private[this] val auralObs        = Ref(Disposable.empty[S#Tx])

//    private[this] var tObs: Disposable[S#Tx] = _

    object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
      def current(implicit tx: S#Tx): Double = -1
    }

//    def factory: Runner.Factory = Runner.Proc

    def init(obj: Proc[S])(implicit tx: S#Tx): this.type = {
      val vOpt = t.getView(obj)
      vOpt.foreach(auralViewAdded)
      // no need to store the observer, as the transport
      // will be disposed with the runner
      /* tObs = */ t.react { implicit tx => {
        case Transport.ViewAdded  (_, v) => auralViewAdded  (v)
        case Transport.ViewRemoved(_, v) => auralViewRemoved(v)
        case _ =>
      }}
      this
    }

    private def auralViewAdded(v: AuralObj[S])(implicit tx: S#Tx): Unit = {
      val ts = targetState()
      val ds = dispatchedState()
      val newObs = v.react { implicit tx => state =>
        val old = dispatchedState.swap(state)
        if (state != old) fire(state)
      }
      auralObs.swap(newObs).dispose()
      auralRef() = Some(v)
      val vs = v.state
      if (ds != vs) {
        dispatchedState() = vs
        fire(vs)
      }
      if (vs != ts) ts match {
        case Runner.Stopped   => v.stop()
        case Runner.Preparing => v.prepare(TimeRef.undefined)
        case Runner.Running   => v.run    (TimeRef.undefined, ())
        case _ =>
      }
    }

    private def auralViewRemoved(v: AuralObj[S])(implicit tx: S#Tx): Unit = {
      auralObs.swap(Disposable.empty).dispose()
      val s   = Runner.Stopped
      val old = dispatchedState.swap(s)
      if (s != old) {
        fire(s)
      }
    }

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
      targetState() = Runner.Preparing
      auralRef().foreach(_.prepare(TimeRef.undefined))
    }

    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = {
      targetState() = Runner.Running
      auralRef().foreach(_.play())
    }

    def stop()(implicit tx: S#Tx): Unit = {
      targetState() = Runner.Stopped
      auralRef().foreach(_.stop())
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      auralObs.swap(Disposable.empty).dispose()
      t.dispose()
    }

    def messages(implicit tx: S#Tx): Any = ???
  }
}