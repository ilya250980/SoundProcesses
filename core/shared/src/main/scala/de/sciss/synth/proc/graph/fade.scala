/*
 *  fade.scala
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

package de.sciss.synth.proc.graph

import de.sciss.proc.UGenGraphBuilder.Input
import de.sciss.proc.{ObjKeys, UGenGraphBuilder}
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.UGenSource.{ProductType, RefMapIn}
import de.sciss.synth.ugen._
import de.sciss.synth.{Curve, GE, Rate, UGenInLike, audio, control}

private[graph] object fade {
  abstract class Base extends GE.Lazy {
    protected def mkEnv(b: UGenGraphBuilder, dur: GE): IEnv

    def rate: Rate

    final def makeUGens: UGenInLike = {
      import de.sciss.synth.geOps
      val b = UGenGraphBuilder.get
      val off     = Offset.ir
      val dur     = Duration.ir
      val phase   = Line.ar(off, dur, dur - off /*, doneAction = freeSelf */)
      val env     = mkEnv(b, dur)
      IEnvGen.ar(env, phase)
    }

    /** Returns (dur, shape, floor) */
    final protected def readCtl(b: UGenGraphBuilder, key: String): (GE, Env.Curve, GE) = {
      val numCh   = b.requestInput(Input.Scalar(key, requiredNumChannels = 4, defaultNumChannels = 4)).numChannels
      assert (numCh == 4)
      // if (numCh != 4) throw new IllegalStateException(s"$this - requires a 4-channel attribute (found $numCh)")
      // b.addAttributeIn(key)
      val ctlName = Attribute.controlName(key)
      val ctl     = ctlName.ir(Seq(0f, 0f, 0f, 0f))  // dur, shape-id, shape-curvature, floor
      import de.sciss.synth.geOps
      (ctl out 0, Env.Curve(ctl out 1, ctl out 2), ctl out 3)
    }
  }

  abstract class SingleBase extends Base {
    protected def key: String

    final protected def mkEnv(b: UGenGraphBuilder, dur: GE): IEnv = {
      val (fadeDur, shape, floor) = readCtl(b, key)
      mkSingleEnv(dur, fadeDur, shape, floor)
    }

    protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Curve, floor: GE): IEnv
  }
}
object FadeIn extends ProductType[FadeIn] {
  override final val typeId = 607

  def kr: FadeIn = kr(ObjKeys.attrFadeIn)
  def kr(key: String): FadeIn = new FadeIn(control, key)

  def ar: FadeIn = ar(ObjKeys.attrFadeIn)
  def ar(key: String): FadeIn = new FadeIn(audio, key)

  override def read(in: RefMapIn, prefix: String, arity: Int): FadeIn = {
    require (arity == 2)
    val _rate   = in.readRate()
    val _key    = in.readString()
    new FadeIn(_rate, _key)
  }
}
final case class FadeIn(rate: Rate, key: String) extends fade.SingleBase {
  protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Curve, floor: GE): IEnv =
    IEnv(floor, Env.Segment(fadeDur, 1, shape) :: Nil)
}

object FadeOut extends ProductType[FadeOut] {
  override final val typeId = 608

  def kr: FadeOut = kr(ObjKeys.attrFadeOut)
  def kr(key: String): FadeOut = new FadeOut(control, key)

  def ar: FadeOut = ar(ObjKeys.attrFadeOut)
  def ar(key: String): FadeOut = new FadeOut(audio, key)

  override def read(in: RefMapIn, prefix: String, arity: Int): FadeOut = {
    require (arity == 2)
    val _rate   = in.readRate()
    val _key    = in.readString()
    new FadeOut(_rate, _key)
  }
}
final case class FadeOut(rate: Rate, key: String) extends fade.SingleBase {
  protected def mkSingleEnv(totalDur: GE, fadeDur: GE, shape: Env.Curve, floor: GE): IEnv = {
    import de.sciss.synth._
    IEnv(1, Env.Segment(fadeDur, floor, shape) :: Nil, totalDur - fadeDur)
  }
}

object FadeInOut extends ProductType[FadeInOut] {
  override final val typeId = 609

  def kr: FadeInOut = kr(ObjKeys.attrFadeIn, ObjKeys.attrFadeOut)
  def kr(inKey: String, outKey: String): FadeInOut = new FadeInOut(control, inKey, outKey)

  def ar: FadeInOut = ar(ObjKeys.attrFadeIn, ObjKeys.attrFadeOut)
  def ar(inKey: String, outKey: String): FadeInOut = new FadeInOut(audio, inKey, outKey)

  override def read(in: RefMapIn, prefix: String, arity: Int): FadeInOut = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _inKey  = in.readString()
    val _outKey = in.readString()
    new FadeInOut(_rate, _inKey, _outKey)
  }
}
final case class FadeInOut(rate: Rate, inKey: String, outKey: String) extends fade.Base {
  protected def mkEnv(b: UGenGraphBuilder, totalDur: GE): IEnv = {
    import de.sciss.synth.geOps
    val (fadeDurIn , shapeIn , floorIn ) = readCtl(b, inKey )
    val (fadeDurOut, shapeOut, floorOut) = readCtl(b, outKey)
    IEnv(floorIn,
      Env.Segment(fadeDurIn, 1, shapeIn) ::
        Env.Segment(totalDur - (fadeDurIn + fadeDurOut), 1, Curve.step) ::
        Env.Segment(fadeDurOut, floorOut, shapeOut) :: Nil
    )
  }
}