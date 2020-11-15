/*
 *  Delay.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.impl.IGeneratorEvent
import de.sciss.lucre.{IEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IControl, ITrigger}
import de.sciss.lucre.Txn.peer
import de.sciss.proc.{ExprContext, Scheduler, TimeRef}

import scala.concurrent.stm.Ref

object Delay {
  /** Creates a new unconnected delay.
    *
    * In order to specify the action taken after the delay, the `apply` method can be used,
    * e.g. `Delay(1.0)(PrintLn("After one second"))`. That is equivalent to writing
    * `val d = Delay(1.0); d ---> PrintLn("After one second"); d`.
    *
    * '''Note:''' There is currently a subtle difference with disposal. If a runner goes into
    * `Stop` state, the delay is cancelled and disposed. If a runner goes into `Done` state,
    * the delay is '''not''' cancelled. This also affects `Action` objects. We will determine
    * in the future whether this behavior is to be kept, or delays are always disposed upon
    * `done`.
    *
    * @param time Delay time in seconds. Negative numbers are clipped to zero.
    */
  def apply(time: Ex[Double]): Delay = Impl(time)

  private final class Expanded[T <: Txn[T]](time: IExpr[T, Double])(implicit protected val targets: ITargets[T],
                                                                    scheduler: Scheduler[T])
    extends Repr[T] with IGeneratorEvent[T, Unit] with IActionImpl[T] {

    private[this] val token = Ref(-1)

    def cancel()(implicit tx: T): Unit = {
      val oldToken = token.swap(-1)
      scheduler.cancel(oldToken)
    }

    def executeAction()(implicit tx: T): Unit = {
      val timeV   = time.value
      val frames  = math.max(0L, (timeV * TimeRef.SampleRate + 0.5).toLong)
//      println(s"dang - timeV $timeV, frames $frames")
      val newToken = scheduler.schedule(scheduler.time + frames) { implicit tx =>
        fire(())
      }
      val oldToken = token.swap(newToken)
      scheduler.cancel(oldToken)
    }

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] =
      Trig.Some

    override def dispose()(implicit tx: T): Unit = {
      // println("DELAY DISPOSE")
      super.dispose()
      cancel()
    }

    def initControl()(implicit tx: T): Unit = ()

    def changed: IEvent[T, Unit] = this
  }

  private final class CancelExpanded[T <: Txn[T]](d: Repr[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      d.cancel()
  }

  final case class Cancel(d: Delay) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Delay$$Cancel" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new CancelExpanded(d.expand[T])
  }

  private final case class Impl(time: Ex[Double]) extends Delay {
    override def productPrefix: String = "Delay" // serialization

    def cancel: Act = Cancel(this)

    def apply(xs: Act*): this.type = {
      xs.foreach { a =>
        this ---> a
      }
      this
    }

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      // Note: we can't just run `Universe()` because Sys is not synth.Sys,
      // and also we may want to preserve the possibility to provide custom schedulers
//      println("EXPAND")
      val ec = ExprContext.get
      import ec.targets
      val u = ec.universe
      import u.scheduler
      new Expanded(time.expand[T])
    }
  }

  trait Repr[T <: Txn[T]] extends IControl[T] with IAction[T] with ITrigger[T] {
    def cancel()(implicit tx: T): Unit
  }
}
/** Delays a trigger by a given amount of time.
  * If a new trigger input arrives before the delay has expired,
  * the previous trigger is cancelled and the delay is rescheduled.
  */
trait Delay extends Control with Act with Trig {
  type Repr[T <: Txn[T]] = Delay.Repr[T]

  /* *Delay time in seconds. */
  def time: Ex[Double]

  def cancel: Act

  /** Convenient way to connect this delay to actions */
  def apply(xs: Act*): this.type
}
