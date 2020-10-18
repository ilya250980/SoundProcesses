/*
 *  Timed.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, ITargets, Txn}
import de.sciss.span.SpanLike

object Timed {
  private final class SpanExpanded[T <: Txn[T], A](in: IExpr[T, Timed[A]], tx0: T)
                                                  (implicit targets: ITargets[T])
    extends MappedIExpr[T, Timed[A], SpanLike](in, tx0) {

    protected def mapValue(inValue: Timed[A])(implicit tx: T): SpanLike = inValue.span
  }

  final case class Span[A](in: Ex[Timed[A]]) extends Ex[SpanLike] {
    type Repr[T <: Txn[T]] = IExpr[T, SpanLike]

    override def productPrefix = s"Timed$$Span"  // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new SpanExpanded(in.expand[T], tx)
    }
  }

  private final class ValueExpanded[T <: Txn[T], A](in: IExpr[T, Timed[A]], tx0: T)
                                                   (implicit targets: ITargets[T])
    extends MappedIExpr[T, Timed[A], A](in, tx0) {

    protected def mapValue(inValue: Timed[A])(implicit tx: T): A = inValue.value
  }

  final case class Value[A](in: Ex[Timed[A]]) extends Ex[A] {
    type Repr[T <: Txn[T]] = IExpr[T, A]

    override def productPrefix = s"Timed$$Value"  // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ValueExpanded(in.expand[T], tx)
    }
  }

  implicit class ExOps[A](private val t: Ex[Timed[A]]) extends AnyVal {
    // def collect[B: Obj.Selector]: Ex[Timed[B]] = ...

    def span  : Ex[SpanLike]  = Span  (t)
    def value : Ex[A]         = Value (t)
  }
}
final case class Timed[+A] private[lucre] (span: SpanLike, value: A)
