package de.sciss.synth.proc

import de.sciss.lucre.expr.ExAttrBridge
import de.sciss.lucre.expr.impl.{ExAttrBridgeImpl => Impl}
import de.sciss.synth.Curve

object ExOps {
  implicit val audioCueExAttrBridge   : ExAttrBridge[AudioCue   ] = new Impl(AudioCue   .Obj)
  implicit val codeExAttrBridge       : ExAttrBridge[Code       ] = new Impl(Code       .Obj)
  implicit val colorExAttrBridge      : ExAttrBridge[Color      ] = new Impl(Color      .Obj)
  implicit val curveExAttrBridge      : ExAttrBridge[Curve      ] = new Impl(CurveObj)
  implicit val envSegmentExAttrBridge : ExAttrBridge[EnvSegment ] = new Impl(EnvSegment .Obj)
  implicit val fadeSpecExAttrBridge   : ExAttrBridge[FadeSpec   ] = new Impl(FadeSpec   .Obj)
//  implicit val fileExAttrBridge       : ExAttrBridge[File       ] = ...
  //  implicit def ExAttrBridge[Markdown]
}
