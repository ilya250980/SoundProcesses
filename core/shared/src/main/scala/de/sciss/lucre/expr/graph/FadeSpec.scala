/*
 *  FadeSpec.scala
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

import de.sciss.lucre.{IExpr, ITargets, Txn}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.Context
import de.sciss.proc.{FadeSpec => _FadeSpec}
import de.sciss.synth.{Curve => _Curve}
import de.sciss.proc.ExImport._

object FadeSpec {
  private final class NumFramesExpanded[T <: Txn[T]](in: IExpr[T, _FadeSpec], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _FadeSpec, Long](in, tx0) {

    protected def mapValue(inValue: _FadeSpec)(implicit tx: T): Long = inValue.numFrames
  }

  final case class NumFrames(in: Ex[_FadeSpec]) extends Ex[Long] {
    override def productPrefix: String = s"FadeSpec$$NumFrames" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Long]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NumFramesExpanded(in.expand[T], tx)
    }
  }

  private final class CurveExpanded[T <: Txn[T]](in: IExpr[T, _FadeSpec], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _FadeSpec, _Curve](in, tx0) {

    protected def mapValue(inValue: _FadeSpec)(implicit tx: T): _Curve = inValue.curve
  }

  final case class Curve(in: Ex[_FadeSpec]) extends Ex[_Curve] {
    override def productPrefix: String = s"FadeSpec$$Curve" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _Curve]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new CurveExpanded(in.expand[T], tx)
    }
  }

  private final class FloorOffsetExpanded[T <: Txn[T]](in: IExpr[T, _FadeSpec], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _FadeSpec, Double](in, tx0) {

    protected def mapValue(inValue: _FadeSpec)(implicit tx: T): Double = inValue.floor
  }

  /** A utility method that reports the offset with respect to the file's sample rate.
    * That is, it multiplies `offset` by the factor `this.sampleRate / TimeRef.SampleRate`
    */
  final case class Floor(in: Ex[_FadeSpec]) extends Ex[Double] {
    override def productPrefix: String = s"FadeSpec$$Floor" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Double]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new FloorOffsetExpanded(in.expand[T], tx)
    }
  }

  private[lucre] final case class ApplyOp[T <: Txn[T]]()
    extends TernaryOp.Op[Long, _Curve, Double, _FadeSpec] {

    override def productPrefix: String = s"FadeSpec$$ApplyOp" // serialization

    def apply(a: Long, b: _Curve, c: Double): _FadeSpec =
      _FadeSpec(a, b, c.toFloat)
  }

  def apply(numFrames: Ex[Long], curve: Ex[_Curve] = _Curve.linear, floor: Ex[Double] = 0.0): Ex[_FadeSpec] =
    Apply(numFrames, curve, floor)

  private final case class Apply(numFrames: Ex[Long],
                                 curve    : Ex[_Curve],
                                 floor    : Ex[Double])
    extends Ex[_FadeSpec] {

    override def productPrefix: String = "FadeSpec" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _FadeSpec]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new TernaryOp.Expanded(ApplyOp[T](), numFrames.expand[T], curve.expand[T], floor.expand[T], tx)
    }
  }
}