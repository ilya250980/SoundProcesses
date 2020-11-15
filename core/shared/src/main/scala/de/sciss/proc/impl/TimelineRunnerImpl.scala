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

package de.sciss.proc.impl

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.{BooleanObj, Obj, Source, synth}
import de.sciss.proc.Runner.{Running, Stopped}
import de.sciss.proc.{Runner, Timeline, Universe}

import scala.concurrent.stm.Ref

object TimelineRunnerImpl {
  def apply[T <: synth.Txn[T]](obj: Timeline[T])(implicit tx: T, universe: Universe[T]): Runner[T] = {
    new Impl(tx.newHandle(obj), universe).init()
  }

  private final class Impl[T <: synth.Txn[T]](objH: Source[T, Timeline[T]],
                                        val universe: Universe[T])
    extends BasicAuralRunnerImpl[T] {

    override def toString = s"Runner.Timeline${hashCode().toHexString}"

    // def tpe: Obj.Type = Timeline

    private[this] val schToken = Ref(-1)

    protected def obj(implicit tx: T): Obj[T] = objH()

    def init()(implicit tx: T): this.type =
      connectAuralSystem()

    private def mkSch()(implicit tx: T): Unit = {
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

    private def cancelSch()(implicit tx: T): Unit = {
      val token = schToken.swap(-1)
      if (token >= 0) universe.scheduler.cancel(token)
    }

    override protected def stateWillChanged(now: Runner.State)(implicit tx: T): Unit = now match {
      case Running  => mkSch()
      case Stopped  => cancelSch()
      case _        =>
    }

    override protected def disposeData()(implicit tx: T): Unit = {
      super.disposeData()
      cancelSch()
    }
  }
}