/*
 *  ControlRunnerImpl.scala
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
import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Obj, Sys, UndoManager}
import de.sciss.synth.proc.Runner.{Attr, Failed, Prepared, Running, Stopped}
import de.sciss.synth.proc.{Control, ExprContext, Runner, Universe}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

object ControlRunnerImpl {
  def apply[S <: Sys[S]](obj: Control[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    new Impl(tx.newHandle(obj))

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Control[S]])(implicit val universe: Universe[S])
    extends BasicRunnerImpl[S] {

    type Repr = Control[S]

    private[this] val ctlRef = Ref(Option.empty[Try[IControl[S]]])

    def tpe: Obj.Type = Control

    def stop()(implicit tx: S#Tx): Unit = {
      disposeData()
      state = Stopped
    }

    override def toString = s"Runner.Control${hashCode().toHexString}"

    protected def disposeData()(implicit tx: S#Tx): Unit =
      ctlRef.swap(None) match {
        case Some(Success(c)) => c.dispose()
        case _ =>
      }

    @tailrec
    def prepare(attr: Attr)(implicit tx: S#Tx): Unit = {
      state match {
        case Stopped  =>
          val tr = mkRef()
          state = tr match {
            case Success(_)   => Prepared
            case Failure(ex)  => Failed(ex)
          }

        case Prepared =>

        case _ => // running or done/failed; go back to square one
          stop()
          prepare(attr)
      }
    }

    @tailrec
    def run()(implicit tx: S#Tx): Unit = {
      state match {
        case Stopped =>
          mkRef()
          runWithRef()

        case Prepared =>
          runWithRef()

        case Running =>

        case _ => // done/failed; go back to square one
          stop()
          run()
      }
    }

    private def runWithRef()(implicit tx: S#Tx): Unit = {
      val trOpt = ctlRef()
      trOpt.foreach { tr =>
        val tr1 = tr.flatMap { c =>
          Try(c.initControl())
        }
        state = tr1 match {
          case Success(_)   => Running
          case Failure(ex)  => Failed(ex)
        }
      }
    }

    private def mkRef()(implicit tx: S#Tx): Try[IControl[S]] = {
      disposeData()
      val ctl   = objH()
      implicit val u: UndoManager[S]  = UndoManager()
      implicit val ctx: Context[S]    = ExprContext(Some(objH))
      val g     = ctl.graph.value
      val res   = Try(g.expand[S])
      ctlRef()  = Some(res)
      res
    }

    object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
      def current(implicit tx: S#Tx): Double = -1
    }
  }
}