/*
 *  AudioCue.scala
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

import de.sciss.file.File
import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.impl.AbstractExObjBridgeImpl
import de.sciss.lucre.expr.{CellView, Context}
import de.sciss.lucre.{Adjunct, IExpr, ITargets, Txn, Obj => LObj}
import de.sciss.serial.DataInput
import de.sciss.synth.io.{AudioFileSpec => _AudioFileSpec}
import de.sciss.synth.proc.{AudioCue => _AudioCue}

object AudioCue {
  private lazy val _init: Unit =
    Adjunct.addFactory(TypeImpl)

  def init(): Unit = _init

  def Type: Obj.Bridge[_AudioCue] with Obj.CanMake[_AudioCue] with HasDefault[_AudioCue] = TypeImpl

  private object TypeImpl extends AbstractExObjBridgeImpl[_AudioCue, _AudioCue, _AudioCue.Obj](_AudioCue.Obj)
    with HasDefault[_AudioCue] with Adjunct.Factory {

    override def toString: String = "AudioCue"

    type A                = _AudioCue
    type _Ex[T <: Txn[T]] = _AudioCue.Obj[T]
    import _AudioCue.{Obj => tpe}

    final val id = 2004

    // WARNING: this must correspond with the serialization of `AbstractExObjBridgeImpl`!
    def readIdentifiedAdjunct(in: DataInput): Adjunct = {
      val typeId = in.readInt()
      assert (typeId == tpe.typeId)
      this
    }

    def defaultValue: _AudioCue = emptyValue

    protected def encode(in: A): A = in

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[A]] =
      CellView.attrUndoOpt[T, A, _Ex](map = obj.attr, key = key)(tx, tpe)
  }

  private val emptyValue =
    _AudioCue(new File(""), _AudioFileSpec(numChannels = 0, sampleRate = 0.0), offset = 0L, gain = 1.0)

  final case class Empty() extends Ex[_AudioCue] {
    override def productPrefix: String = s"AudioCue$$Empty" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _AudioCue]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(emptyValue)
  }

  private final class ArtifactExpanded[T <: Txn[T]](in: IExpr[T, _AudioCue], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioCue, File](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: T): File = inValue.artifact
  }

  final case class Artifact(in: Ex[_AudioCue]) extends Ex[File] {
    override def productPrefix: String = s"AudioCue$$Artifact" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, File]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ArtifactExpanded(in.expand[T], tx)
    }
  }

  private final class SpecExpanded[T <: Txn[T]](in: IExpr[T, _AudioCue], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioCue, _AudioFileSpec](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: T): _AudioFileSpec = inValue.spec
  }

  final case class Spec(in: Ex[_AudioCue]) extends Ex[_AudioFileSpec] {
    override def productPrefix: String = s"AudioCue$$Spec" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _AudioFileSpec]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new SpecExpanded(in.expand[T], tx)
    }
  }

  private final class OffsetExpanded[T <: Txn[T]](in: IExpr[T, _AudioCue], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioCue, Long](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: T): Long = inValue.offset
  }

  final case class Offset(in: Ex[_AudioCue]) extends Ex[Long] {
    override def productPrefix: String = s"AudioCue$$Offset" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Long]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new OffsetExpanded(in.expand[T], tx)
    }
  }

  private final class GainExpanded[T <: Txn[T]](in: IExpr[T, _AudioCue], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioCue, Double](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: T): Double = inValue.gain
  }

  final case class Gain(in: Ex[_AudioCue]) extends Ex[Double] {
    override def productPrefix: String = s"AudioCue$$Gain" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Double]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new GainExpanded(in.expand[T], tx)
    }
  }

  private final class FileOffsetExpanded[T <: Txn[T]](in: IExpr[T, _AudioCue], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioCue, Long](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: T): Long = inValue.fileOffset
  }

  /** A utility method that reports the offset with respect to the file's sample rate.
    * That is, it multiplies `offset` by the factor `this.sampleRate / TimeRef.SampleRate`
    */
  final case class FileOffset(in: Ex[_AudioCue]) extends Ex[Long] {
    override def productPrefix: String = s"AudioCue$$FileOffset" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Long]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new FileOffsetExpanded(in.expand[T], tx)
    }
  }

  private[lucre] final case class ApplyOp[T <: Txn[T]]()
    extends QuaternaryOp.Op[File, _AudioFileSpec, Long, Double, _AudioCue] {

    override def productPrefix: String = s"AudioCue$$ApplyOp" // serialization

    def apply(a: File, b: _AudioFileSpec, c: Long, d: Double): _AudioCue =
      _AudioCue(a, b, c, d)
  }

  def apply(artifact: Ex[File], spec: Ex[_AudioFileSpec],
            offset: Ex[Long] = 0L, gain: Ex[Double] = 1.0): Ex[_AudioCue] =
    Apply(artifact, spec, offset, gain)

  private final case class Apply(artifact : Ex[File],
                                 spec     : Ex[_AudioFileSpec],
                                 offset   : Ex[Long],
                                 gain     : Ex[Double])
    extends Ex[_AudioCue] {

    override def productPrefix: String = "AudioCue" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _AudioCue]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new QuaternaryOp.Expanded(ApplyOp[T](),
        artifact.expand[T], spec.expand[T], offset.expand[T], gain.expand[T], tx)
    }
  }
}