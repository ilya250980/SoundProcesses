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
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Sys, UndoManager}
import de.sciss.synth.proc.Runner.{Attr, Failed, Prepared, Running, Stopped}
import de.sciss.synth.proc.{Control, ExprContext, Runner, Universe}

import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

object ControlRunnerImpl {
  def apply[S <: Sys[S]](obj: Control[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
    new Impl(tx.newHandle(obj))

  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Control[S]])(implicit val universe: Universe[S])
    extends BasicRunnerInternalImpl[S] {

    type Repr = Control[S]

    private type Ctl = (Try[IControl[S]], Context[S])

    private[this] val ctlRef  = Ref(Option.empty[Ctl])
    private[this] val attrRef = Ref(Context.emptyAttr[S])(NoManifest)

    // XXX TODO --- should unify Runner and ObjViewBase
    // def tpe: Obj.Type = Control

    def stop()(implicit tx: S#Tx): Unit = {
      disposeData()
      state = Stopped
    }

    override def toString = s"Runner.Control${hashCode().toHexString}"

    override protected def disposeData()(implicit tx: S#Tx): Unit = {
      super.disposeData()
      attrRef() = Context.emptyAttr
      disposeCtl()
    }

    private def disposeCtl()(implicit tx: S#Tx): Unit =
      ctlRef.swap(None) match {
        case Some((tr, ctx)) =>
          tr.foreach(_.dispose())
          ctx.dispose()
        case _ =>
      }

    def prepare(attr: Attr[S])(implicit tx: S#Tx): Unit = {
      def ok(): Unit = {
        attrRef() = attr
        val tr    = mkRef()
        state = tr match {
          case Success(_)   => Prepared
          case Failure(ex)  => Failed(ex)
        }
      }

      state match {
        case Stopped  => ok()
        case Prepared =>

        case _ => // running or done/failed; go back to square one
          // stop()
          // prepare(attr)
          disposeData()
          ok()
      }
    }

    def run()(implicit tx: S#Tx): Unit = {
      def ok(): Unit = {
        mkRef()
        runWithRef()
      }

      state match {
        case Stopped  => ok()
        case Prepared => runWithRef()

        case Running =>

        case _ => // done/failed; go back to square one
//          stop()
//          run()
          disposeData()
          ok()
      }
    }

    private def runWithRef()(implicit tx: S#Tx): Unit = {
      val ctlOpt = ctlRef()
      ctlOpt.foreach { case (tr, _) =>
        state = Running
        val tr1 = tr.flatMap { c =>
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
    }

    private def mkRef()(implicit tx: S#Tx): Try[IControl[S]] = {
      disposeCtl()
      val ctl   = objH()
      implicit val u: UndoManager[S]  = UndoManager()
      val attr  = attrRef()
      implicit val ctx: Context[S]    = ExprContext(Some(objH), attr, Some(this))
      val g     = ctl.graph.value
      val res   = Try(g.expand[S])
      ctlRef()  = Some((res, ctx))
      res
    }
  }
}
