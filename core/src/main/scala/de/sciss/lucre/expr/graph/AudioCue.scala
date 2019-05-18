/*
 *  AudioCue.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.file.File
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change
import de.sciss.synth.io.{AudioFileSpec => _AudioFileSpec}
import de.sciss.synth.proc.{AudioCue => _AudioCue}

object AudioCue {
  private final class ArtifactExpanded[S <: Sys[S]](in: IExpr[S, _AudioCue], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioCue, File](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: S#Tx): File = inValue.artifact
  }

  final case class Artifact(in: Ex[_AudioCue]) extends Ex[File] {
    override def productPrefix: String = s"AudioCue$$Artifact" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, File]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ArtifactExpanded(in.expand[S], tx)
    }
  }

  private final class SpecExpanded[S <: Sys[S]](in: IExpr[S, _AudioCue], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioCue, _AudioFileSpec](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: S#Tx): _AudioFileSpec = inValue.spec
  }

  final case class Spec(in: Ex[_AudioCue]) extends Ex[_AudioFileSpec] {
    override def productPrefix: String = s"AudioCue$$Spec" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, _AudioFileSpec]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new SpecExpanded(in.expand[S], tx)
    }
  }

  private final class OffsetExpanded[S <: Sys[S]](in: IExpr[S, _AudioCue], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioCue, Long](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: S#Tx): Long = inValue.offset
  }

  final case class Offset(in: Ex[_AudioCue]) extends Ex[Long] {
    override def productPrefix: String = s"AudioCue$$Offset" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Long]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new OffsetExpanded(in.expand[S], tx)
    }
  }

  private final class GainExpanded[S <: Sys[S]](in: IExpr[S, _AudioCue], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioCue, Double](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: S#Tx): Double = inValue.gain
  }

  final case class Gain(in: Ex[_AudioCue]) extends Ex[Double] {
    override def productPrefix: String = s"AudioCue$$Gain" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Double]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new GainExpanded(in.expand[S], tx)
    }
  }

  private final class FileOffsetExpanded[S <: Sys[S]](in: IExpr[S, _AudioCue], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioCue, Long](in, tx0) {

    protected def mapValue(inValue: _AudioCue)(implicit tx: S#Tx): Long = inValue.fileOffset
  }

  /** A utility method that reports the offset with respect to the file's sample rate.
    * That is, it multiplies `offset` by the factor `this.sampleRate / TimeRef.SampleRate`
    */
  final case class FileOffset(in: Ex[_AudioCue]) extends Ex[Long] {
    override def productPrefix: String = s"AudioCue$$FileOffset" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Long]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new FileOffsetExpanded(in.expand[S], tx)
    }
  }

  private final class ApplyExpanded[S <: Sys[S]](artifact : IExpr[S, File],
                                                 spec     : IExpr[S, _AudioFileSpec],
                                                 offset   : IExpr[S, Long],
                                                 gain     : IExpr[S, Double], tx0: S#Tx)
                                                (implicit protected val targets: ITargets[S])
    extends IExpr[S, _AudioCue] with IEventImpl[S, Change[_AudioCue]] {

    artifact.changed.--->(this)(tx0)
    spec    .changed.--->(this)(tx0)
    offset  .changed.--->(this)(tx0)
    gain    .changed.--->(this)(tx0)

    def value(implicit tx: S#Tx): _AudioCue =
      _AudioCue(artifact.value, spec.value, offset.value, gain.value)

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[_AudioCue]] = {
      def pullCh[A](ex: IExpr[S, A]): Change[A] = {
        val evt = ex.changed
        def default: Change[A] = {
          val v = ex.value; Change(v, v)
        }
        if (pull.contains(evt)) pull(evt).getOrElse(default) else default
      }

      val aCh = pullCh(artifact)
      val sCh = pullCh(spec    )
      val oCh = pullCh(offset  )
      val gCh = pullCh(gain    )
      val ch  = Change(
        _AudioCue(aCh.before, sCh.before, oCh.before, gCh.before),
        _AudioCue(aCh.now   , sCh.now   , oCh.now   , gCh.now   )
      )
      if (ch.isSignificant) Some(ch) else None
    }

    def changed: IEvent[S, Change[_AudioCue]] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      artifact.changed.-/->(this)
      spec    .changed.-/->(this)
      offset  .changed.-/->(this)
      gain    .changed.-/->(this)
    }
  }

  def apply(artifact: Ex[File], spec: Ex[_AudioFileSpec],
            offset: Ex[Long] = 0L, gain: Ex[Double] = 1.0): Ex[_AudioCue] =
    Apply(artifact, spec, offset, gain)

  private final case class Apply(artifact: Ex[File], spec: Ex[_AudioFileSpec],
                                 offset: Ex[Long], gain: Ex[Double]) extends Ex[_AudioCue] {
    override def productPrefix: String = "AudioCue" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, _AudioCue]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded(artifact.expand[S], spec.expand[S], offset.expand[S], gain.expand[S], tx)
    }
  }
}