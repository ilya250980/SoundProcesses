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

import de.sciss.lucre.expr
import de.sciss.lucre.expr.ExElem
import de.sciss.synth.UGenSource

/** Registers UGen and Ex graph elements proper to SoundProcesses.  */
object ProcElem {
  private lazy val _init: Unit = {
    UGenSource.addProductReaderSq (synthSq)
    UGenSource.addProductReaders  (synthMap)
    ExElem    .addProductReaderSq (exSq)
  }

  private type ExV = ExElem.ProductReader[Product]

  private def exSq: Seq[ExV] = {
    import expr.graph._
    Seq[ExV](
      ThisRunner, ThisRunner.Stop, ThisRunner.Done, ThisRunner.Fail, ThisRunner.Progress,
      ThisRunner.Attr, ThisRunner.Attr.Update, ThisRunner.Attr.Set,
    )
  }

  private type SynthV = UGenSource.ProductReader[Product]

  private def synthSq: Seq[SynthV] = {
    import graph._
    Seq[SynthV](
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
  }

  private def synthMap: Map[String, SynthV] = {
    import graph._
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
