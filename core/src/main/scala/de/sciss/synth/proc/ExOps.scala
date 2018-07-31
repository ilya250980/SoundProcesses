/*
 *  ExOps.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.expr.graph.Attr
import de.sciss.lucre.expr.impl.{ExAttrBridgeImpl => Impl}
import de.sciss.synth.Curve

object ExOps {
  implicit val audioCueExAttrBridge   : Attr.Bridge[AudioCue   ] = new Impl(AudioCue   .Obj)
  implicit val codeExAttrBridge       : Attr.Bridge[Code       ] = new Impl(Code       .Obj)
  implicit val colorExAttrBridge      : Attr.Bridge[Color      ] = new Impl(Color      .Obj)
  implicit val curveExAttrBridge      : Attr.Bridge[Curve      ] = new Impl(CurveObj)
  implicit val envSegmentExAttrBridge : Attr.Bridge[EnvSegment ] = new Impl(EnvSegment .Obj)
  implicit val fadeSpecExAttrBridge   : Attr.Bridge[FadeSpec   ] = new Impl(FadeSpec   .Obj)
//  implicit val fileExAttrBridge       : ExAttr.Bridge[File       ] = ...
  //  implicit def ExAttrBridge[Markdown]
}
