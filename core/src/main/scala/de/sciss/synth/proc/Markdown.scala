/*
 *  Markdown.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.lucre.{Expr, Ident, Txn, Var => LVar}
import de.sciss.serial.TFormat
import de.sciss.synth.proc

object Markdown extends ExprTypeImpl[String, Markdown] {
  import proc.{Markdown => Repr}

  final val typeId      = 29
  final val valueFormat = TFormat.String

  /** Boolean indicating whether view should go into edit mode by default. */
  final val attrEditMode    = "edit-mode"

  def tryParse(value: Any): Option[String] = value match {
    case x: String  => Some(x)
    case _          => None
  }

  protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                  (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private[this] final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  type Value = String
}

/** The markdown type is more or less the same
  * as a `StringObj`, but with dedicated type id.
  * In the future, we will have to evaluate whether
  * this makes sense and whether we need bridge
  * methods between `Markdown <=> StringObj`.
  *
  * Markdown links and inserted objects are found
  * through the object's attribute map.
  */
trait Markdown[T <: Txn[T]] extends Expr[T, String]
