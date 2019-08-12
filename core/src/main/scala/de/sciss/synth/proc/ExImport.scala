/*
 *  ExOps.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.expr.graph.{AudioFileSpec, Ex, Obj}
import de.sciss.lucre.expr.impl.{ExObjBridgeImpl => Impl}
import de.sciss.synth.proc

object ExImport {
  // XXX TODO
//  implicit val audioCueExAttrBridge   : Obj.Bridge[AudioCue   ] = new Impl(AudioCue   .Obj)
//  implicit val codeExAttrBridge       : Obj.Bridge[Code       ] = new Impl(Code       .Obj)
//  implicit val colorExAttrBridge      : Obj.Bridge[Color      ] = new Impl(Color      .Obj)
//  implicit val curveExAttrBridge      : Obj.Bridge[Curve      ] = new Impl(CurveObj)
//  implicit val envSegmentExAttrBridge : Obj.Bridge[EnvSegment ] = new Impl(EnvSegment .Obj)
//  implicit val fadeSpecExAttrBridge   : Obj.Bridge[FadeSpec   ] = new Impl(FadeSpec   .Obj)

  implicit object curveIsValue          extends Ex.Value[Curve]
  implicit object audioFileSpecIsValue  extends Ex.Value[AudioFileSpec]

  type AudioCue       = proc              .AudioCue
  type AudioFileSpec  = de.sciss.synth.io .AudioFileSpec
  type FadeSpec       = proc              .FadeSpec
  type Curve          = de.sciss.synth    .Curve

  implicit final class audioFileSpecOps(private val x: Ex[AudioFileSpec]) extends AnyVal {
    def numChannels : Ex[Int    ] = AudioFileSpec.NumChannels(x)
    def numFrames   : Ex[Long   ] = AudioFileSpec.NumFrames  (x)
    def sampleRate  : Ex[Double ] = AudioFileSpec.SampleRate (x)
  }

//  implicit final class procTrigOps(private val x: Trig) extends AnyVal {
//    def delay(time: Ex[Double]): Delay
//  }
}

