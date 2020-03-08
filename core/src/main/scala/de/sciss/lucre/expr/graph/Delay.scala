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

import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr, ITrigger}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.synth.proc.{ExprContext, Scheduler, TimeRef}

import scala.concurrent.stm.Ref

object Delay {
  /**
    * @param time Delay time in seconds. Negative numbers are clipped to zero.
    */
  def apply(time: Ex[Double]): Delay = Impl(time)

  private final class Expanded[S <: Sys[S]](time: IExpr[S, Double])(implicit protected val targets: ITargets[S],
                                                                    scheduler: Scheduler[S])
    extends Repr[S] with IGenerator[S, Unit] with IActionImpl[S] {

    private[this] val token = Ref(-1)

    def cancel()(implicit tx: S#Tx): Unit = {
      val oldToken = token.swap(-1)
      scheduler.cancel(oldToken)
    }

    def executeAction()(implicit tx: S#Tx): Unit = {
      val timeV   = time.value
      val frames  = math.max(0L, (timeV * TimeRef.SampleRate + 0.5).toLong)
//      println(s"dang - timeV $timeV, frames $frames")
      val newToken = scheduler.schedule(scheduler.time + frames) { implicit tx =>
        fire(())
      }
      val oldToken = token.swap(newToken)
      scheduler.cancel(oldToken)
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] =
      Trig.Some

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      cancel()
    }

    def changed: IEvent[S, Unit] = this
  }

  private final class CancelExpanded[S <: Sys[S]](d: Repr[S]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit =
      d.cancel()
  }

  final case class Cancel(d: Delay) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"Delay$$Cancel" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new CancelExpanded(d.expand[S])
  }

  private final case class Impl(time: Ex[Double]) extends Delay {
    override def productPrefix: String = "Delay" // serialization

    def cancel: Act = Cancel(this)

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      // Note: we can't just run `Universe()` because Sys is not synth.Sys,
      // and also we may want to preserve the possibility to provide custom schedulers
//      println("EXPAND")
      val ec = ExprContext.get
      import ec.targets
      val u = ec.universe
      import u.scheduler
      new Expanded(time.expand[S])
    }
  }

  trait Repr[S <: Sys[S]] extends ITrigger[S] with IAction[S] {
    def cancel()(implicit tx: S#Tx): Unit
  }
}
/** Delays a trigger by a given amount of time.
  * If a new trigger input arrives before the delay has expired,
  * the previous trigger is cancelled and the delay is rescheduled.
  */
trait Delay extends Act with Trig {
  type Repr[S <: Sys[S]] = Delay.Repr[S]

  /* *Delay time in seconds. */
  def time: Ex[Double]

  def cancel: Act
}
