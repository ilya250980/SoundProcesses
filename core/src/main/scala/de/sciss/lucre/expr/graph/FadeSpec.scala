/*
 *  FadeSpec.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.{FadeSpec => _FadeSpec}
import de.sciss.synth.{Curve => _Curve}
import de.sciss.synth.proc.ExImport._

object FadeSpec {
  private final class NumFramesExpanded[S <: Sys[S]](in: IExpr[S, _FadeSpec], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _FadeSpec, Long](in, tx0) {

    protected def mapValue(inValue: _FadeSpec)(implicit tx: S#Tx): Long = inValue.numFrames
  }

  final case class NumFrames(in: Ex[_FadeSpec]) extends Ex[Long] {
    override def productPrefix: String = s"FadeSpec$$NumFrames" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Long]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NumFramesExpanded(in.expand[S], tx)
    }
  }

  private final class CurveExpanded[S <: Sys[S]](in: IExpr[S, _FadeSpec], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _FadeSpec, _Curve](in, tx0) {

    protected def mapValue(inValue: _FadeSpec)(implicit tx: S#Tx): _Curve = inValue.curve
  }

  final case class Curve(in: Ex[_FadeSpec]) extends Ex[_Curve] {
    override def productPrefix: String = s"FadeSpec$$Curve" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, _Curve]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new CurveExpanded(in.expand[S], tx)
    }
  }

  private final class FloorOffsetExpanded[S <: Sys[S]](in: IExpr[S, _FadeSpec], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _FadeSpec, Double](in, tx0) {

    protected def mapValue(inValue: _FadeSpec)(implicit tx: S#Tx): Double = inValue.floor
  }

  /** A utility method that reports the offset with respect to the file's sample rate.
    * That is, it multiplies `offset` by the factor `this.sampleRate / TimeRef.SampleRate`
    */
  final case class Floor(in: Ex[_FadeSpec]) extends Ex[Double] {
    override def productPrefix: String = s"FadeSpec$$Floor" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Double]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new FloorOffsetExpanded(in.expand[S], tx)
    }
  }

  private[lucre] final case class ApplyOp[S <: Sys[S]]()
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

    type Repr[S <: Sys[S]] = IExpr[S, _FadeSpec]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new TernaryOp.Expanded(ApplyOp[S](), numFrames.expand[S], curve.expand[S], floor.expand[S], tx)
    }
  }
}