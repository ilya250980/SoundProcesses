/*
 *  AudioFileSpec.scala
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
import de.sciss.synth.io.{AudioFile, AudioFileSpec => _AudioFileSpec}

import scala.util.Try

object AudioFileSpec {
  private final class NumChannelsExpanded[S <: Sys[S]](in: IExpr[S, _AudioFileSpec], tx0: S#Tx)
                                                      (implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioFileSpec, Int](in, tx0) {

    protected def mapValue(inValue: _AudioFileSpec)(implicit tx: S#Tx): Int = inValue.numChannels
  }

  final case class NumChannels(in: Ex[_AudioFileSpec]) extends Ex[Int] {
    override def productPrefix: String = s"AudioFileSpec$$NumChannels" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Int]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NumChannelsExpanded(in.expand[S], tx)
    }
  }

  private final class NumFramesExpanded[S <: Sys[S]](in: IExpr[S, _AudioFileSpec], tx0: S#Tx)
                                                    (implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioFileSpec, Long](in, tx0) {

    protected def mapValue(inValue: _AudioFileSpec)(implicit tx: S#Tx): Long = inValue.numFrames
  }

  final case class NumFrames(in: Ex[_AudioFileSpec]) extends Ex[Long] {
    override def productPrefix: String = s"AudioFileSpec$$NumFrames" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Long]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NumFramesExpanded(in.expand[S], tx)
    }
  }

  private final class SampleRateExpanded[S <: Sys[S]](in: IExpr[S, _AudioFileSpec], tx0: S#Tx)
                                                    (implicit targets: ITargets[S])
    extends MappedIExpr[S, _AudioFileSpec, Double](in, tx0) {

    protected def mapValue(inValue: _AudioFileSpec)(implicit tx: S#Tx): Double = inValue.sampleRate
  }

  final case class SampleRate(in: Ex[_AudioFileSpec]) extends Ex[Double] {
    override def productPrefix: String = s"AudioFileSpec$$SampleRate" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Double]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new SampleRateExpanded(in.expand[S], tx)
    }
  }

  def read(in: Ex[File]): Ex[Option[_AudioFileSpec]] = Read(in)

  private final class ReadExpanded[S <: Sys[S]](in: IExpr[S, File], tx0: S#Tx)
                                               (implicit targets: ITargets[S])
    extends MappedIExpr[S, File, Option[_AudioFileSpec]](in, tx0) {

    protected def mapValue(inValue: File)(implicit tx: S#Tx): Option[_AudioFileSpec] =
      Try(
        AudioFile.readSpec(inValue)
      ).toOption
  }

  def Empty(): Ex[_AudioFileSpec] = Const(_AudioFileSpec(numChannels = 0, sampleRate = 0.0))

  final case class Read(in: Ex[File]) extends Ex[Option[_AudioFileSpec]] {
    override def productPrefix: String = s"AudioFileSpec$$Read" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[_AudioFileSpec]]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ReadExpanded(in.expand[S], tx)
    }
  }
}
