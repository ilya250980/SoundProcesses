/*
 *  AudioFileSpec.scala
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

import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, graph}
import de.sciss.lucre.{IExpr, ITargets, Txn}
import de.sciss.synth.io.{AudioFileType, SampleFormat, AudioFileSpec => _AudioFileSpec}

import scala.annotation.switch

object AudioFileSpec extends AudioFileSpecPlatform {
  private final class NumChannelsExpanded[T <: Txn[T]](in: IExpr[T, _AudioFileSpec], tx0: T)
                                                      (implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioFileSpec, Int](in, tx0) {

    protected def mapValue(inValue: _AudioFileSpec)(implicit tx: T): Int = inValue.numChannels
  }

  final case class NumChannels(in: Ex[_AudioFileSpec]) extends Ex[Int] {
    override def productPrefix: String = s"AudioFileSpec$$NumChannels" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Int]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NumChannelsExpanded(in.expand[T], tx)
    }
  }

  private final class NumFramesExpanded[T <: Txn[T]](in: IExpr[T, _AudioFileSpec], tx0: T)
                                                    (implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioFileSpec, Long](in, tx0) {

    protected def mapValue(inValue: _AudioFileSpec)(implicit tx: T): Long = inValue.numFrames
  }

  final case class NumFrames(in: Ex[_AudioFileSpec]) extends Ex[Long] {
    override def productPrefix: String = s"AudioFileSpec$$NumFrames" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Long]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new NumFramesExpanded(in.expand[T], tx)
    }
  }

  private final class SampleRateExpanded[T <: Txn[T]](in: IExpr[T, _AudioFileSpec], tx0: T)
                                                    (implicit targets: ITargets[T])
    extends MappedIExpr[T, _AudioFileSpec, Double](in, tx0) {

    protected def mapValue(inValue: _AudioFileSpec)(implicit tx: T): Double = inValue.sampleRate
  }

  final case class SampleRate(in: Ex[_AudioFileSpec]) extends Ex[Double] {
    override def productPrefix: String = s"AudioFileSpec$$SampleRate" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Double]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new SampleRateExpanded(in.expand[T], tx)
    }
  }

  // Note: we cannot use serializer `_AudioFileSpec` without further ado, because we have
  // singleton objects like `AIFF`.
//  def Empty(): Ex[_AudioFileSpec] = Const(_AudioFileSpec(numChannels = 0, sampleRate = 0.0))

  final case class Empty() extends Ex[_AudioFileSpec] {
    override def productPrefix: String = s"AudioFileSpec$$Empty" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _AudioFileSpec]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded(_AudioFileSpec(numChannels = 0, sampleRate = 0.0))
  }

  /** Creates a new `AudioFileSpec` expression.
    *
    * @param fileType       the file-type can be 0 (AIFF), 1 (Wave), 2 (Wave64), 3 (IRCAM), 4 (NeXT), or 5 (Raw).
    *                       It defaults to AIFF, and invalid values are also mapped to AIFF.
    * @param sampleFormat   the sample-format can be 0 (16-bit int), 1 (24-bit int), 2 (32-bit float), 3 (32-bit int),
    *                       4 (64-bit float), 5 (unsigned 8-bit int), 6 (signed 8-bit int).
    *                       It defaults to 32-bit float, and invalid values are also mapped to 32-bit float.
    * @param numFrames      The number of sample frames. Often this value is not used, e.g. when writing to a new
    *                       file. The default is zero.
    */
  def apply(fileType     : Ex[Int]  = 0,
            sampleFormat : Ex[Int]  = 2,
            numChannels  : Ex[Int],
            sampleRate   : Ex[Double],
            numFrames    : Ex[Long] = 0L,
           ): Ex[_AudioFileSpec] =
    Apply(fileType = fileType, sampleFormat = sampleFormat, numChannels = numChannels,
      sampleRate = sampleRate, numFrames = numFrames)

  // XXX TODO DRY with FScape
//  private def id(in: AudioFileType): Int = in match {
//    case AudioFileType.AIFF    => 0
//    case AudioFileType.Wave    => 1
//    case AudioFileType.Wave64  => 2
//    case AudioFileType.IRCAM   => 3
//    case AudioFileType.NeXT    => 4
//    case AudioFileType.Raw     => 5
//    case other => sys.error(s"Unexpected audio file type $other")
//  }

//  private def id(in: SampleFormat): Int = in match {
//    case SampleFormat.Int16    => 0
//    case SampleFormat.Int24    => 1
//    case SampleFormat.Float    => 2
//    case SampleFormat.Int32    => 3
//    case SampleFormat.Double   => 4
//    case SampleFormat.UInt8    => 5
//    case SampleFormat.Int8     => 6
//    case other => sys.error(s"Unexpected sample format $other")
//  }

  private def fileType(id: Int): AudioFileType = (id: @switch) match {
    case 0 => AudioFileType.AIFF
    case 1 => AudioFileType.Wave
    case 2 => AudioFileType.Wave64
    case 3 => AudioFileType.IRCAM
    case 4 => AudioFileType.NeXT
    case 5 => AudioFileType.Raw
    case _ /*other*/ => AudioFileType.AIFF // sys.error(s"Unexpected audio file type id $other")
  }

  private def sampleFormat(id: Int): SampleFormat = (id: @switch) match {
    case 0 => SampleFormat.Int16
    case 1 => SampleFormat.Int24
    case 2 => SampleFormat.Float
    case 3 => SampleFormat.Int32
    case 4 => SampleFormat.Double
    case 5 => SampleFormat.UInt8
    case 6 => SampleFormat.Int8
    case _ /*other*/ => SampleFormat.Int16 // sys.error(s"Unexpected sample format id $other")
  }

  private final case class ApplyOp() extends QuinaryOp.Op[Int, Int, Int, Double, Long, _AudioFileSpec] {
    override def apply(fileTypeId: Int, sampleFormatId: Int, numChannels: Int, sampleRate: Double,
                       numFrames: Long): _AudioFileSpec = {
      val ft = fileType     (fileTypeId)
      val sf = sampleFormat (sampleFormatId)
      _AudioFileSpec(ft, sf, numChannels = numChannels, sampleRate = sampleRate, numFrames = numFrames)
    }
  }

  private final case class Apply(fileType     : Ex[Int],
                                 sampleFormat : Ex[Int],
                                 numChannels  : Ex[Int],
                                 sampleRate   : Ex[Double],
                                 numFrames    : Ex[Long],
                                ) extends Ex[_AudioFileSpec] {

    override def productPrefix: String = "AudioFileSpec"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _AudioFileSpec]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val fileTypeEx      = fileType    .expand[T]
      val sampleFormatEx  = sampleFormat.expand[T]
      val numChannelsEx   = numChannels .expand[T]
      val sampleRateEx    = sampleRate  .expand[T]
      val numFramesEx     = numFrames   .expand[T]
      import ctx.targets
      new graph.QuinaryOp.Expanded(ApplyOp(),
        fileTypeEx, sampleFormatEx, numChannelsEx, sampleRateEx, numFramesEx, tx)
    }
  }
}
