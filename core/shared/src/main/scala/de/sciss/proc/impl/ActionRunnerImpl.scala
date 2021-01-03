/*
 *  ActionRunnerImpl.scala
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

import de.sciss.lucre.expr.{Context, IAction, IControl}
import de.sciss.lucre.{Source, Txn}
import de.sciss.proc.Runner.{Done, Failed}
import de.sciss.proc.{Action, Runner, Universe}

import scala.util.{Failure, Success, Try}

object ActionRunnerImpl {
  def apply[T <: Txn[T]](obj: Action[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
    new Impl(tx.newHandle(obj))

  private final class Impl[T <: Txn[T]](objH: Source[T, Action[T]])(implicit universe: Universe[T])
    extends BasicControlRunnerImpl[T, IAction[T] with IControl[T]](objH) {

    override def toString = s"Runner.Action${hashCode().toHexString}"

    protected def run(tr: Try[IRepr])(implicit tx: T): Unit = {
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

    protected def expandGraph()(implicit tx: T, ctx: Context[T]): IRepr = {
      val ctl = objH()
      val g   = ctl.graph.value
      g.expand[T]
    }
  }
}
