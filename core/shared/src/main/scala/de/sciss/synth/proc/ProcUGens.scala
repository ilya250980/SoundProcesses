/*
 *  ProcUGens.scala
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

package de.sciss.synth.proc

import de.sciss.synth.UGenSource
import de.sciss.synth.UGenSource.ProductReader

/** Registers UGen graph elements proper to SoundProcesses.  */
object ProcUGens {
  private lazy val _init: Unit = {
    UGenSource.addProductReaderSq (seq)
    UGenSource.addProductReaders  (map)
  }

  type V = ProductReader[Product]

  import graph._

  private def seq: Seq[V] = Seq[V](
    Action,
    Reaction,
    Attribute,
    Buffer,
    BufferGen,
    BufferOut,
    FadeIn, FadeOut, FadeInOut,
    ScanIn, ScanOut, ScanInFix,
    StopSelf,
    DiskIn, VDiskIn, DiskOut, BufChannels, BufRateScale, BufSampleRate,
    Time, Offset, Duration,
  )

  private def map: Map[String, V] = {
    import BufferGen._
    Map(
      (Cheby.readerKey, Command),
      (Copy .readerKey, Command),
      (Sine1.readerKey, Command),
      (Sine2.readerKey, Command),
      (Sine3.readerKey, Command),
    )
  }

  def init(): Unit = _init
}
