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
      AudioCue, AudioCue.Empty, AudioCue.Artifact, AudioCue.Spec, AudioCue.Offset, AudioCue.Gain, AudioCue.FileOffset,
      AudioFileSpec, AudioFileSpec.NumChannels, AudioFileSpec.NumFrames, AudioFileSpec.SampleRate, AudioFileSpec.Read,
      AudioFileSpec.Empty,
      Bounce,
      Calendar, Calendar.Trunc, Calendar.Set, Calendar.Add, Calendar.Get, Calendar.Schedule,
      Color.Predef,
      Delay, Delay.Cancel,
      FadeSpec, FadeSpec.NumFrames, FadeSpec.Curve, FadeSpec.Floor,
      File.TmpDir, File.MkDir, File.Delete, File.List,
      OscNode.Dump, OscNode.Codec, OscUdpNode, OscUdpNode.Received, OscUdpNode.Sender, OscUdpNode.Message,
      OscUdpNode.Send,
      /* OscPacket: */ OscMessage, OscMessage.Name, OscMessage.Args, OscMessage.Select,
      Proc, Proc.Tape,
      Runner, Runner.Messages, Runner.Progress, Runner.State, Runner.Stop, Runner.RunWith, Runner.Run,
      SocketAddress, SocketAddress.LocalHost, SocketAddress.Host, SocketAddress.Port,
      Sys.Process, Sys.Process.Directory, Sys.Process.Output, Sys.Exit, Sys.Property, Sys.Env,
      ThisRunner, ThisRunner.Stop, ThisRunner.Done, ThisRunner.Fail, ThisRunner.Progress,
      ThisRunner.Attr, ThisRunner.Attr.Update, ThisRunner.Attr.Set,
      Timed.Span, Timed.Value,
      Timeline, Timeline.Add, Timeline.AddAll, Timeline.Remove, Timeline.Split, Timeline.Split.Left,
      Timeline.Split.Right,
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
      /* Stream: */ DiskIn, DiskIn.Done, VDiskIn, VDiskIn.Done, DiskOut, BufChannels, BufRateScale, BufSampleRate,
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
