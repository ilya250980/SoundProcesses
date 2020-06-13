/*
 *  ControlRunnerImpl.scala
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

import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.Runner.{Failed, Running}
import de.sciss.synth.proc.{Control, Runner, Universe}

import scala.util.{Failure, Success, Try}

object ControlRunnerImpl {
  def apply[S <: Sys[S]](obj: Control[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    new Impl(tx.newHandle(obj))

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Control[S]])(implicit universe: Universe[S])
    extends BasicControlRunnerImpl[S, IControl[S]](objH) {

    override def toString = s"Runner.Control${hashCode().toHexString}"

    protected def run(tr: Try[IRepr])(implicit tx: S#Tx): Unit = {
      val tr1 = tr.flatMap { c =>
        state = Running
        Try(c.initControl())
      }
      tr1 match {
        // do not set Running here; we do that initially (above),
        // and if the logic stops `ThisRunner`, the state
        // will already have been set.
        case Success(_)   => // Running
        case Failure(ex)  => state = Failed(ex)
      }
    }

    protected def expandGraph()(implicit tx: S#Tx, ctx: Context[S]): IRepr = {
      val ctl = objH()
      val g   = ctl.graph.value
      g.expand[S]
    }
  }
}
