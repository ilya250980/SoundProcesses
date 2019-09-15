/*
 *  ExprContext.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.impl.ContextMixin
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Obj, Sys, UndoManager}

object ExprContext {
  // XXX TODO: merge with overloaded method in next major version
  def apply[S <: Sys[S]](selfH: Option[stm.Source[S#Tx, Obj[S]]] = None,
                         attr: Context.Attr[S] = Context.emptyAttr[S])
                        (implicit universe: Universe[S], undoManager: UndoManager[S]): Context[S] =
    new Impl[S](selfH, attr, runner = None)

  def apply[S <: Sys[S]](selfH: Option[stm.Source[S#Tx, Obj[S]]], attr: Context.Attr[S],
                         runner: Option[Runner.Internal[S]])
                        (implicit universe: Universe[S], undoManager: UndoManager[S]): Context[S] =
    new Impl[S](selfH, attr, runner)

  def get[S <: Sys[S]](implicit ctx: Context[S]): ExprContext[S] = ctx match {
    case ec: ExprContext[S] => ec
    case _ => sys.error("Trying to expand graph outside of SoundProcesses context")
  }

  private final class Impl[S <: Sys[S]](protected val selfH: Option[stm.Source[S#Tx, Obj[S]]],
                                        val attr: Context.Attr[S], override val runner: Option[Runner.Internal[S]])
                                       (implicit val universe: Universe[S], val undoManager: UndoManager[S])
    extends ContextMixin[S] with ExprContext[S] {

    // XXX TODO: remove in next major version
    def this(selfH: Option[stm.Source[S#Tx, Obj[S]]], attr: Context.Attr[S])
            (implicit universe: Universe[S], undoManager: UndoManager[S]) =
      this(selfH, attr, runner = None)

    implicit def cursor   : Cursor    [S] = universe.cursor
    implicit def workspace: Workspace [S] = universe.workspace
  }
}
trait ExprContext[S <: Sys[S]] extends Context[S] {
  implicit def universe: Universe[S]

  def runner: Option[Runner.Internal[S]] = None
}
