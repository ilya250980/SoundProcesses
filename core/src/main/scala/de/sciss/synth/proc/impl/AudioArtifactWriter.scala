///*
// *  AudioArtifactWriter.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//package impl
//
//import de.sciss.lucre.synth.{AudioBus, Buffer, Synth, Resource, Txn}
//import de.sciss.synth.{addToHead, ControlSet, SynthGraph}
//import de.sciss.span.Span
//import de.sciss.numbers
//
//object AudioArtifactWriter {
//  /** Creates a new audio artifact streaming synth.
//    *
//    * @param bus          the bus to write the output signal to
//    * @param segm         the segment containing the audio grapheme
//    * @param frame         the current time with respect to the outer timeline (`Timeline.SampleRate`)
//    */
//  def apply(bus: AudioBus, segm: Grapheme.Segment.Audio, frame: Long)
//           (implicit tx: Txn): Resource = {
//    val numChannels       = segm.numChannels
//    val serverSampleRate  = bus.server.sampleRate
//    val sg  = SynthGraph {
//      import de.sciss.synth._
//      import ugen._
//      val buf = "buf".ir
//      val dur = "dur".ir(1f)
//      val out = "out".kr
//      val amp = "amp".kr(1f)
//      val sig0 = if (segm.value.spec.sampleRate == serverSampleRate) {
//        DiskIn .ar(numChannels, buf)
//      } else {
//        VDiskIn.ar(numChannels, buf, speed = "speed".ir(1f))
//      }
//      val sig = sig0 * amp
//      Line.kr(start = 0, end = 0, dur = dur, doneAction = freeSelf)
//      Out.ar(out, sig)
//    }
//    val synth = Synth(bus.server, sg, nameHint = Some("audio-artifact"))
//    val res   = new Impl(synth = synth, bus = bus, segm = segm, frameTL = frame, serverSampleRate = serverSampleRate)
//    res.play()
//    res
//  }
//
//  private final class Impl(synth: Synth, bus: AudioBus, segm: Grapheme.Segment.Audio, frameTL: Long,
//                           serverSampleRate: Double)
//    extends Resource.Proxy {
//
//    // def resource(implicit tx: Txn) = synth
//
//    protected def resourcePeer: Resource = synth
//
//    def play()(implicit tx: Txn): Unit = {
//      val audioVal  = segm.value
//      val file      = audioVal.artifact
//      val path      = file.getAbsolutePath
//      val spec      = audioVal.spec
//      val fileFrames= spec.numFrames
//      val factor    = spec.sampleRate / serverSampleRate
//      // XXX TODO - the target might come from an AudioContext eventually;
//      // this is fine nevertheless, because all we need to ensure is that
//      // the synth plays at the head
//      val offsetTL  = frameTL - segm.span.start
//      val srFile    = spec.sampleRate
//      val srRatio   = srFile / TimeRef.SampleRate
//      val offsetFile = offsetTL * srRatio
//      val target    = server.defaultGroup
//      val startFile = math.max(0L, math.min(fileFrames, audioVal.offset + (offsetFile + 0.5).toLong))
//      val stopFile  = segm.span match {
//        case Span.HasStop(stopTL) =>
//          val lenTL   = stopTL - frameTL
//          val stop0   = startFile + (lenTL * srRatio + 0.5).toLong
//          math.min(stop0, fileFrames)
//        case _ => fileFrames
//      }
//      val dur       = (stopFile - startFile) / srFile
//      import numbers.Implicits._
//      // println(f"AudioArtifactWriter. fStart = $fStart, fStop = $fStop, $dur = $dur%1.3f")
//      val bufSize   = (Buffer.defaultCueBufferSize * factor).toInt.nextPowerOfTwo max (server.peer.config.blockSize << 1)
//      val rb        = Buffer.diskIn(server)(path, startFrame = startFile, numChannels = bus.numChannels, numFrames = bufSize)
//      val args0     = List[ControlSet]("buf" -> rb.id, "dur" -> dur, "amp" -> audioVal.gain)
//      val args      = if (factor == 1.0) args0 else ("speed" -> factor: ControlSet) :: args0
//
//      synth.play(target = target, args = args, addAction = addToHead, dependencies = rb :: Nil)
//
//      synth.onEndTxn { implicit tx =>
//        rb.dispose()
//      }
//
//      synth.write(bus -> "out")
//    }
//
//    // def remove()(implicit tx: Txn): Unit = if (synth.isOnline) synth.free()
//  }
//}