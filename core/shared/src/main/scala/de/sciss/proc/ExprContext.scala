/*
 *  ExprContext.scala
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

package de.sciss.proc

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.impl.ContextMixin
import de.sciss.lucre.{Cursor, Obj, Source, Txn, Workspace => LWorkspace}

object ExprContext {
  def apply[T <: Txn[T]](selfH: Option[Source[T, Obj[T]]] = None,
                         attr: Context.Attr[T] = Context.emptyAttr[T], runner: Option[Runner.Internal[T]] = None)
                        (implicit universe: Universe[T], undoManager: UndoManager[T]): Context[T] =
    new Impl[T](selfH, attr, runner)

  def get[T <: Txn[T]](implicit ctx: Context[T]): ExprContext[T] = ctx match {
    case ec: ExprContext[T] => ec
    case _ => sys.error("Trying to expand graph outside of SoundProcesses context")
  }

  private final class Impl[T <: Txn[T]](protected val selfH: Option[Source[T, Obj[T]]],
                                        val attr: Context.Attr[T], override val runner: Option[Runner.Internal[T]])
                                       (implicit val universe: Universe[T], val undoManager: UndoManager[T])
    extends ContextMixin[T] with ExprContext[T] {

    implicit def cursor   : Cursor    [T] = universe.cursor
    implicit def workspace: LWorkspace[T] = universe.workspace
  }
}
trait ExprContext[T <: Txn[T]] extends Context[T] {
  implicit def universe: Universe[T]

  def runner: Option[Runner.Internal[T]] = None
}
