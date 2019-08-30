/*
 *  ActionRunnerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.expr.{Context, IAction, IControl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Obj, Sys, UndoManager}
import de.sciss.synth.proc.Runner.{Attr, Done, Failed, Prepared, Running, Stopped}
import de.sciss.synth.proc.{Action, ExprContext, Runner, Universe}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

object ActionRunnerImpl {
  def apply[S <: Sys[S]](obj: Action[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    new Impl(tx.newHandle(obj))

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Action[S]])(implicit val universe: Universe[S])
    extends BasicRunnerImpl[S] {

    type Repr = Action[S]

    private[this] val actRef  = Ref(Option.empty[Try[IAction[S] with IControl[S]]])
    private[this] val attrRef = Ref(Context.emptyAttr[S])(NoManifest)

    def tpe: Obj.Type = Action

    def stop()(implicit tx: S#Tx): Unit = {
      disposeData()
      state = Stopped
    }

    override def toString = s"Runner.Action${hashCode().toHexString}"

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      attrRef() = Context.emptyAttr
      disposeCtl()
    }

    private def disposeCtl()(implicit tx: S#Tx): Unit =
      actRef.swap(None) match {
        case Some(Success(c)) => c.dispose()
        case _ =>
      }

    @tailrec
    def prepare(attr: Attr[S])(implicit tx: S#Tx): Unit = {
      state match {
        case Stopped  =>
          attrRef() = attr
          val tr    = mkRef()
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
      val trOpt = actRef()
      trOpt.foreach { tr =>
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
    }

    private def mkRef()(implicit tx: S#Tx): Try[IAction[S]] = {
      disposeCtl()
      val ctl   = objH()
      implicit val u: UndoManager[S]  = UndoManager()
      val attr  = attrRef()
      implicit val ctx: Context[S]    = ExprContext(Some(objH), attr)
      val g     = ctl.graph.value
      val res   = Try(g.expand[S])
      actRef()  = Some(res)
      res
    }

    object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
      def current(implicit tx: S#Tx): Double = -1
    }
  }
}
