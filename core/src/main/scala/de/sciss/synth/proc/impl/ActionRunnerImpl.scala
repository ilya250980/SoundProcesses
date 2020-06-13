/*
 *  ActionRunnerImpl.scala
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

import de.sciss.lucre.expr.{Context, IAction, IControl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Runner.{Done, Failed}
import de.sciss.synth.proc.{Action, Runner, Universe}

import scala.util.{Failure, Success, Try}

object ActionRunnerImpl {
  def apply[S <: Sys[S]](obj: Action[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    new Impl(tx.newHandle(obj))

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Action[S]])(implicit universe: Universe[S])
    extends BasicControlRunnerImpl[S, IAction[S] with IControl[S]](objH) {

    override def toString = s"Runner.Action${hashCode().toHexString}"

    protected def run(tr: Try[IRepr])(implicit tx: S#Tx): Unit = {
      val tr1 = tr.flatMap { c =>
        Try {
          c.initControl()
          c.executeAction()
        }
      }
      state = tr1 match {
        case Success(_)   => Done // Running
        case Failure(ex)  => Failed(ex)
      }
    }

    protected def expandGraph()(implicit tx: S#Tx, ctx: Context[S]): IRepr = {
      val ctl = objH()
      val g   = ctl.graph.value
      g.expand[S]
    }
  }
}
