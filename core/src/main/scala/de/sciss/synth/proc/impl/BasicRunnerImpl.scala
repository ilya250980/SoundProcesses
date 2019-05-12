/*
 *  BasicRunnerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Sys, Workspace}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.{AuralObj, ObjViewBase, Proc, Runner, TimeRef, Transport}

import scala.concurrent.stm.Ref

trait BasicRunnerImpl[S <: Sys[S]]
  extends BasicViewBaseImpl[S, Unit] with Runner[S] {

  implicit final def workspace : Workspace[S] = universe.workspace
  implicit final def cursor    : Cursor[S]    = universe.cursor

  def initControl()(implicit tx: S#Tx): Unit = ()

  final object messages extends Runner.Messages[S#Tx] with ObservableImpl[S, List[Runner.Message]] {
    private[this] val ref = Ref(List.empty[Runner.Message])

    def current(implicit tx: S#Tx): List[Runner.Message] = ref()

    def current_=(value: List[Runner.Message])(implicit tx: S#Tx): Unit = {
      ref() = value
      fire(value)
    }
  }
}

object BasicRunnerImpl {
  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx, universe: Runner.Universe[S]): Runner[S] = {
    // the transport is simply to get the work done of dynamically creating
    // an aural-obj... a bit of a resource waste?
    val t = Transport[S](universe)
    t.addObject(obj)
    new Impl(tx.newHandle(obj), t, universe).initRunner(obj)
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Obj[S]],
                                        t: Transport[S],
                                        val universe: Runner.Universe[S])
    extends BasicRunnerImpl[S] with ObjViewBase[S, Unit] {

    override def toString = s"Runner.Proc${hashCode().toHexString}"

    def tpe: Obj.Type = Proc

    private[this] val dispatchedState = Ref[Runner.State](Runner.Stopped)
    private[this] val targetState     = Ref[Runner.State](Runner.Stopped)
    private[this] val auralRef        = Ref(Option.empty[AuralObj[S]])
    private[this] val auralObs        = Ref(Disposable.empty[S#Tx])

    object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
      def current(implicit tx: S#Tx): Double = -1
    }

    def initRunner(obj: Obj[S])(implicit tx: S#Tx): this.type = {
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
  }
}