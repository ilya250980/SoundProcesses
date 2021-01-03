/*
 *  AuralControlImpl.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.Context
import de.sciss.lucre.{Disposable, Obj, Source, Txn, synth}
import de.sciss.proc.{AuralContext, AuralObj, Control, ExprContext, Runner, TimeRef, Universe}

import scala.concurrent.stm.Ref
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object AuralControlImpl extends AuralObj.Factory {
  type Repr[T <: Txn[T]]  = Control[T]
  def tpe: Obj.Type       = Control

  def apply[T <: synth.Txn[T]](obj: Control[T], attr: Runner.Attr[T])
                         (implicit tx: T, context: AuralContext[T]): AuralObj.Control[T] = {
    val objH = tx.newHandle(obj)
    new Impl(objH, attr)
  }

  private final class Impl[T <: synth.Txn[T]](objH: Source[T, Control[T]], attr: Runner.Attr[T])
                                        (implicit context: AuralContext[T])
    extends BasicViewBaseImpl[T] with AuralObj.Control[T] {

    implicit def universe: Universe[T] = context.universe

    private[this] val ctlCtxRef = Ref[Disposable[T]](Disposable.empty)

    override type Repr = Control[T]

    def tpe: Obj.Type = Control

    override def obj(implicit tx: T): Control[T] = objH()

    def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit =
      state = Runner.Prepared

    // XXX TODO DRY with ControlRunnerImpl
    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: T): Unit = {
      val ctl = objH()
      implicit val u: UndoManager[T]  = UndoManager()
      implicit val ctx: Context[T]    = ExprContext(Some(objH), attr, None) // XXX TODO --- we lose Runner.Internal here
      val g   = ctl.graph.value
      val ct  = Try(g.expand[T])
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

    def stop()(implicit tx: T): Unit = {
      disposeCtlCtx()
      state = Runner.Stopped
    }

    private def disposeCtlCtx()(implicit tx: T): Unit =
      ctlCtxRef.swap(Disposable.empty).dispose()

    def dispose()(implicit tx: T): Unit =
      disposeCtlCtx()
  }
}