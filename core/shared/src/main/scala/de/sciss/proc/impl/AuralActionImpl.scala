/*
 *  AuralActionImpl.scala
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

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.Context
import de.sciss.lucre.{Obj, Source, Txn, synth}
import de.sciss.proc.{Action, AuralContext, AuralObj, ExprContext, Runner, TimeRef, Universe}

import scala.util.control.NonFatal

object AuralActionImpl extends AuralObj.Factory {
  type Repr[T <: Txn[T]]  = Action[T]
  def tpe: Obj.Type       = Action

  def apply[T <: synth.Txn[T]](obj: Action[T], attr: Runner.Attr[T])
                         (implicit tx: T, context: AuralContext[T]): AuralObj.Action[T] = {
    val objH = tx.newHandle(obj)
    new Impl(objH, attr)
  }

  private final class Impl[T <: synth.Txn[T]](objH: Source[T, Action[T]], attr: Runner.Attr[T])
                                        (implicit context: AuralContext[T])
    extends BasicViewBaseImpl[T] with AuralObj.Action[T] {

    implicit def universe: Universe[T] = context.universe

    override type Repr = Action[T]

    def tpe: Obj.Type = Action

    override def obj(implicit tx: T): Action[T] = objH()

    def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit =
      state = Runner.Prepared

    // XXX TODO DRY with ActionRunnerImpl
    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: T): Unit = {
      val ctl = objH()
      implicit val u: UndoManager[T]  = UndoManager()
      implicit val ctx: Context[T]    = ExprContext(Some(objH), attr, None) // XXX TODO --- we lose Runner.Internal here
      val g = ctl.graph.value
      try {
        val c = g.expand[T]
        try {
          c.initControl()
          state = Runner.Running
          c.executeAction()
          state = Runner.Stopped
        } finally {
          c.dispose()
        }
      } catch {
        case NonFatal(ex) =>
          state = Runner.Failed(ex)
      }
    }

    override def toString = s"AuralAction@${hashCode().toHexString}"

    def stop()(implicit tx: T): Unit =
      state = Runner.Stopped

    def dispose()(implicit tx: T): Unit = ()
  }
}