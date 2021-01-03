/*
 *  ControlRunnerImpl.scala
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

import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.{Source, Txn}
import de.sciss.proc.Runner.{Failed, Running}
import de.sciss.proc.{Control, Runner, Universe}

import scala.util.{Failure, Success, Try}

object ControlRunnerImpl {
  def apply[T <: Txn[T]](obj: Control[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
    new Impl(tx.newHandle(obj))

  private final class Impl[T <: Txn[T]](objH: Source[T, Control[T]])(implicit universe: Universe[T])
    extends BasicControlRunnerImpl[T, IControl[T]](objH) {

    override def toString = s"Runner.Control${hashCode().toHexString}"

    protected def run(tr: Try[IRepr])(implicit tx: T): Unit = {
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

    protected def expandGraph()(implicit tx: T, ctx: Context[T]): IRepr = {
      val ctl = objH()
      val g   = ctl.graph.value
      g.expand[T]
    }
  }
}
