/*
 *  TimelineRunnerImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.expr.BooleanObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Runner.{Running, Stopped}
import de.sciss.synth.proc.{Runner, Timeline, Universe}

import scala.concurrent.stm.Ref

object TimelineRunnerImpl {
  def apply[S <: Sys[S]](obj: Timeline[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] = {
    new Impl(tx.newHandle(obj), universe).init()
  }

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Timeline[S]],
                                        val universe: Universe[S])
    extends BasicAuralRunnerImpl[S] {

    override def toString = s"Runner.Timeline${hashCode().toHexString}"

    // def tpe: Obj.Type = Timeline

    private[this] val schToken = Ref(-1)

    protected def obj(implicit tx: S#Tx): Obj[S] = objH()

    def init()(implicit tx: S#Tx): this.type =
      connectAuralSystem()

    private def mkSch()(implicit tx: S#Tx): Unit = {
      val tl = objH()
      tl.lastEvent.foreach { timeStop =>
        val now = 0L  // XXX TODO timeRef.force.span.start
        val dt  = timeStop - now
        if (dt > 0) {
          val sch   = universe.scheduler
          val timeS = sch.time
          val token = sch.schedule(timeS + dt) { implicit tx =>
            val tl    = objH()
            val loop  = tl.attr.$[BooleanObj]("loop").exists(_.value) // bit of a hack LOL
            if (loop) {
              stop()
              run()
            }
          }
          cancelSch()
          schToken() = token
        }
      }
    }

    private def cancelSch()(implicit tx: S#Tx): Unit = {
      val token = schToken.swap(-1)
      if (token >= 0) universe.scheduler.cancel(token)
    }

    override protected def stateWillChanged(now: Runner.State)(implicit tx: S#Tx): Unit = now match {
      case Running  => mkSch()
      case Stopped  => cancelSch()
      case _        =>
    }

    override protected def disposeData()(implicit tx: S#Tx): Unit = {
      super.disposeData()
      cancelSch()
    }
  }
}