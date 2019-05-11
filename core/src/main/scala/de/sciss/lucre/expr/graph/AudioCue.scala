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
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.io.{AudioFileSpec => _AudioFileSpec}
import de.sciss.synth.proc.{AudioCue => _AudioCue}

object AudioCue {
  private final class ArtifactExpanded[S <: Sys[S]](in: IExpr[S, _AudioCue], tx0: S#Tx)(implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioCue, File](in, tx0) {

    protected def mapValue(inValue: _AudioCue): File = inValue.artifact
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

    protected def mapValue(inValue: _AudioCue): _AudioFileSpec = inValue.spec
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

    protected def mapValue(inValue: _AudioCue): Long = inValue.offset
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

    protected def mapValue(inValue: _AudioCue): Double = inValue.gain
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

    protected def mapValue(inValue: _AudioCue): Long = inValue.fileOffset
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
}