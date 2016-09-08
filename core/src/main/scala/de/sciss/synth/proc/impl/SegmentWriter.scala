///*
// *  SegmentWriter.scala
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
//import de.sciss.lucre.synth.{AudioBus, Resource, Synth, Txn}
//import de.sciss.synth.Curve.parametric
//import de.sciss.synth.{ControlSet, SynthGraph, addToHead}
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//
//object SegmentWriter {
//  def apply(bus: AudioBus, segm: Grapheme.Segment.Curve, time: Long, sampleRate: Double)
//           (implicit tx: Txn): Resource = {
//
//    val (usesShape, sg) = graph(segm)
//    val synth = Synth(bus.server, sg, nameHint = Some("grapheme-segm"))
//    // val bus   = RichBus.audio(server, segm.numChannels)
//    val res = new Impl(synth, usesShape, bus, segm, time, sampleRate)
//    res.play()
//    res
//  }
//
//  private def graph(segm: Grapheme.Segment.Curve): (Boolean, SynthGraph) = {
//    var usesShape = false // XXX TODO dirty variable
//    val sg = SynthGraph {
//      import de.sciss.synth._
//      import de.sciss.synth.ugen._
//
//      val zero        = Vec.fill(segm.numChannels)(0f)
//      val start       = "start".ir(zero)
//      val stop        = "stop" .ir(zero)
//      val dur         = "dur".ir
//
//      lazy val shape      = "shape".ir(zero)
//      lazy val curvature  = "curve".ir(zero)
//
//      val sig: GE = segm.values.zipWithIndex.map {
//        case ((segmStart, segmStop, segmShape), ch) =>
//          val doneAction = if (ch == 0) freeSelf else doNothing
//          segmShape match {
//            case Curve.linear =>
//              Line.ar(start, stop, dur, doneAction = doneAction)
//            case Curve.exponential =>
//              if (segmStart != 0f && segmStop != 0f && segmStart * segmStop > 0f) {
//                XLine.ar(start, stop, dur, doneAction = doneAction)
//              } else {
//                Line.ar(0, 0, dur, doneAction = doneAction)
//              }
//            case _ =>
//              usesShape = true
//              val curve = Env.Curve(shape \ ch, curvature \ ch)
//              val env   = Env(start, Env.Segment(dur = dur, targetLevel = stop, curve = curve) :: Nil)
//              EnvGen.ar(env, doneAction = doneAction)
//          }
//      }
//      // sig.poll(4)
//      Out.ar("out".kr, sig)
//    }
//    (usesShape, sg)
//  }
//
//  private final class Impl(synth: Synth, usesShape: Boolean, bus: AudioBus,
//                           segm: Grapheme.Segment.Curve, time: Long, sampleRate: Double)
//    extends Resource.Proxy {
//
//    protected def resourcePeer: Resource = synth
//
//    def play()(implicit tx: Txn): Unit = {
//      type Ctl = List[ControlSet]
//
//      val target    = server.defaultGroup // XXX
//      val durSecs   = segm.span.length / sampleRate
//      val (vStart, vStop, vShape) = segm.values.unzip3
//      val ctl0: Ctl = List("start" -> vStart.map(_.toFloat), "stop" -> vStop.map(_.toFloat), "dur" -> durSecs)
//      val ctl1: Ctl = if (usesShape) "shape" -> vShape.map(_.id.toFloat) :: ctl0 else ctl0
//      val args: Ctl = if (usesShape) "curve" -> vShape.map { case parametric(c) => c; case _ => 0f } :: ctl1 else ctl1
//
//      synth.play(target = target, args = args, addAction = addToHead, dependencies = Nil)
//
//      synth.write(bus -> "out")
//    }
//  }
//}