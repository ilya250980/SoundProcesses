/*
 *  AuralControlImpl.scala
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

import de.sciss.lucre.expr.Context
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Obj, Sys, UndoManager}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.{AuralContext, AuralObj, Control, ExprContext, Runner, TimeRef, Universe}

import scala.concurrent.stm.Ref
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object AuralControlImpl extends AuralObj.Factory {
  type Repr[S <: Sys[S]]  = Control[S]
  def tpe: Obj.Type       = Control

  def apply[S <: SSys[S]](obj: Control[S], attr: Runner.Attr[S])
                         (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Control[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH, attr)
  }

  private final class Impl[S <: SSys[S]](objH: stm.Source[S#Tx, Control[S]], attr: Runner.Attr[S])
                                        (implicit context: AuralContext[S])
    extends BasicViewBaseImpl[S] with AuralObj.Control[S] {

    implicit def universe: Universe[S] = context.universe

    private[this] val ctlCtxRef = Ref[Disposable[S#Tx]](Disposable.empty)

    override type Repr = Control[S]

    def tpe: Obj.Type = Control

    override def obj(implicit tx: S#Tx): Control[S] = objH()

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit =
      state = Runner.Prepared

    // XXX TODO DRY with ControlRunnerImpl
    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = {
      val ctl = objH()
      implicit val u: UndoManager[S]  = UndoManager()
      implicit val ctx: Context[S]    = ExprContext(Some(objH), attr, None) // XXX TODO --- we lose Runner.Internal here
      val g   = ctl.graph.value
      val ct  = Try(g.expand[S])
      ctlCtxRef.swap(ctx).dispose()
      ct match {
        case Success(c) =>
          state = Runner.Running
          try {
            c.initControl()
          } catch {
            case NonFatal(ex) =>
              disposeCtlCtx()
              state = Runner.Failed(ex)
          }
        case Failure(ex) =>
          disposeCtlCtx()
          state = Runner.Failed(ex)
      }
    }

    override def toString = s"AuralControl@${hashCode().toHexString}"

    def stop()(implicit tx: S#Tx): Unit = {
      disposeCtlCtx()
      state = Runner.Stopped
    }

    private def disposeCtlCtx()(implicit tx: S#Tx): Unit =
      ctlCtxRef.swap(Disposable.empty).dispose()

    def dispose()(implicit tx: S#Tx): Unit =
      disposeCtlCtx()
  }
}