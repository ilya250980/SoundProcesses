/*
 *  ActionRunnerImpl.scala
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

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.synth.proc
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Runner.{Attr, Prepared, Running, Stopped}
import de.sciss.synth.proc.{Action, ObjViewBase, Runner, TimeRef, Universe}

/** The action runner supports passing a value in by
  * using the entry `"value"` in the `prepare` method's `attr` argument.
  */
object ActionRunnerImpl {
  def apply[S <: Sys[S]](obj: Action[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    new Impl(tx.newHandle(obj), universe)

  abstract class Base[S <: Sys[S], Target] extends ObjViewBase[S, Target] with BasicViewBaseImpl[S] {
    // ---- abstract ----

    implicit def universe: proc.Universe[S]

    type Repr = Action[S]

    // ---- impl ----

    final def tpe: Obj.Type = Action

    final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
      state = Prepared
    }

    final def stop()(implicit tx: S#Tx): Unit =
      state = Stopped

//    def run(timeRef: TimeRef.Option, target: Target)(implicit tx: S#Tx): Unit =

    final protected def execute(invokeValue: Any)(implicit tx: S#Tx): Unit = {
      state = Running
      val action = obj
      if (!action.muted) {
        val au = Action.Universe[S](action, value = invokeValue)
        action.execute(au)
      }
      state = Stopped
    }
  }

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Action[S]], override val universe: Universe[S])
    extends Base[S, Unit] with BasicRunnerImpl[S] {

    override def toString = s"Runner.Action${hashCode().toHexString}"

    protected def disposeData()(implicit tx: S#Tx): Unit = ()

    override def obj(implicit tx: S#Tx): Action[S] = objH()

    def prepare(attr: Attr)(implicit tx: S#Tx): Unit = ???

    def run()(implicit tx: S#Tx): Unit =
      execute(???)

    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit =
      execute(())

    object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
      def current(implicit tx: S#Tx): Double = -1
    }
  }
}
