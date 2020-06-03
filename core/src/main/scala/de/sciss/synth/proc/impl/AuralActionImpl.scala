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

package de.sciss.synth.proc.impl

import de.sciss.lucre.expr.Context
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys, UndoManager}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.{Action, AuralContext, AuralObj, ExprContext, Runner, TimeRef, Universe}

import scala.util.control.NonFatal

object AuralActionImpl extends AuralObj.Factory {
  type Repr[S <: Sys[S]]  = Action[S]
  def tpe: Obj.Type       = Action

  def apply[S <: SSys[S]](obj: Action[S], attr: Runner.Attr[S])
                         (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH, attr)
  }

  private final class Impl[S <: SSys[S]](objH: stm.Source[S#Tx, Action[S]], attr: Runner.Attr[S])
                                        (implicit context: AuralContext[S])
    extends BasicViewBaseImpl[S] with AuralObj.Action[S] {

    implicit def universe: Universe[S] = context.universe

    override type Repr = Action[S]

    def tpe: Obj.Type = Action

    override def obj(implicit tx: S#Tx): Action[S] = objH()

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit =
      state = Runner.Prepared

    // XXX TODO DRY with ActionRunnerImpl
    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = {
      val ctl = objH()
      implicit val u: UndoManager[S]  = UndoManager()
      implicit val ctx: Context[S]    = ExprContext(Some(objH), attr, None) // XXX TODO --- we lose Runner.Internal here
      val g = ctl.graph.value
      try {
        val c = g.expand[S]
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

    def stop()(implicit tx: S#Tx): Unit =
      state = Runner.Stopped

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}