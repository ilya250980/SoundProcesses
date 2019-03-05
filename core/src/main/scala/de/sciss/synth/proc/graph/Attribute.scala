/*
 *  Attribute.scala
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

package de.sciss.synth
package proc
package graph

import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.{AudioControlProxy, ControlProxy, ControlValues}

import scala.collection.immutable.{IndexedSeq => Vec}

object Attribute {
  final class Factory(val `this`: String) extends AnyVal { me =>
    import me.{`this` => name}

    /** Creates an attribute without defaults (attribute must be present). */
    def ir: Attribute = Attribute.ir(key = name)
    /** Creates an attribute with defaults (attribute may be absent). */
    def ir(values: ControlValues): Attribute = Attribute.ir(key = name, default = values)

    /** Creates an attribute without defaults (attribute must be present). */
    def kr: Attribute = Attribute.kr(key = name)
    /** Creates an attribute with defaults (attribute may be absent). */
    def kr(values: ControlValues): Attribute = Attribute.kr(key = name, default = values)

//    /** Creates a trigger attribute without defaults (attribute must be present). */
//    def tr: TrigAttribute = Attribute.tr(key = name)
//    /** Creates an attribute with defaults (attribute may be absent). */
//    def tr(values: ControlValues): TrigAttribute = Attribute.tr(key = name, default = values)

    /** Creates a trigger attribute without defaults (attribute must be present). */
    def ar: Attribute = Attribute.ar(key = name)
    /** Creates an attribute with defaults (attribute may be absent). */
    def ar(values: ControlValues): Attribute = Attribute.ar(key = name, default = values)
  }

  def controlName(key: String): String = s"$$at_$key"

  def ir(key: String): Attribute =
    apply(scalar, key, None, fixed = -1)

  def ir(key: String, fixed: Int): Attribute =
    apply(scalar, key, None, fixed = fixed)

  def ir(key: String, default: ControlValues): Attribute =
    mk(scalar, key, default, fixed = false)

  def ir(key: String, default: ControlValues, fixed: Boolean): Attribute =
    mk(scalar, key, default, fixed = fixed)

  def kr(key: String): Attribute =
    apply(control, key, None, fixed = -1)

  def kr(key: String, fixed: Int): Attribute =
    apply(control, key, None, fixed = fixed)

  def kr(key: String, default: ControlValues): Attribute =
    mk(control, key, default, fixed = false)

  def kr(key: String, default: ControlValues, fixed: Boolean): Attribute =
    mk(control, key, default, fixed = fixed)

  def ar(key: String): Attribute =
    apply(audio, key, None, fixed = -1)

  def ar(key: String, fixed: Int): Attribute =
    apply(audio, key, None, fixed = fixed)

  def ar(key: String, default: ControlValues): Attribute =
    mk(audio, key, default, fixed = false)

  def ar(key: String, default: ControlValues, fixed: Boolean): Attribute =
    mk(audio, key, default, fixed = fixed)

//  def tr(key: String): TrigAttribute =
//    TrigAttribute(key, None, fixed = -1)
//
//  def tr(key: String, fixed: Int): TrigAttribute =
//    TrigAttribute(key, None, fixed = fixed)
//
//  def tr(key: String, default: ControlValues): TrigAttribute =
//    mkTr(key, default, fixed = false)
//
//  def tr(key: String, default: ControlValues, fixed: Boolean): TrigAttribute =
//    mkTr(key, default, fixed = fixed)

  private def mk(rate: Rate, key: String, default: ControlValues, fixed: Boolean): Attribute =
    Attribute(rate, key, Some(default.seq), fixed = if (fixed) default.seq.size else -1)

//  private def mkTr(key: String, default: ControlValues, fixed: Boolean): TrigAttribute =
//    TrigAttribute(key, Some(default.seq), fixed = if (fixed) default.seq.size else -1)

  private[graph] def mkValues(key: String, default: Option[Vec[Float]], fixed: Int): Vec[Float] = {
    val b           = UGenGraphBuilder.get
    val defChannels = default.fold(-1)(_.size)
    val inValue     = b.requestInput(Input.Scalar(
      name                = key,
      requiredNumChannels = fixed,
      defaultNumChannels  = defChannels))
    val numCh       = inValue.numChannels
    val values      = default.fold(Vector.fill(numCh)(0f))(df => Vector.tabulate(numCh)(idx => df(idx % defChannels)))
    values
  }
}
final case class Attribute(rate: Rate, key: String, default: Option[Vec[Float]], fixed: Int)
  extends GE.Lazy {

  def makeUGens: UGenInLike = {
    val values  = Attribute.mkValues(key = key, default = default, fixed = fixed)
    val nameOpt = Some(Attribute.controlName(key))
    val ctl     = if (rate == audio)
      AudioControlProxy(values, nameOpt)
    else
      ControlProxy(rate, values, nameOpt)
    ctl.expand
  }
}

//final case class TrigAttribute(key: String, default: Option[Vec[Float]], fixed: Int)
//  extends GE.Lazy with ControlRated {
//
//  def makeUGens: UGenInLike = {
//    val values  = Attribute.mkValues(key = key, default = default, fixed = fixed)
//    val nameOpt = Some(Attribute.controlName(key))
//    val ctl     = TrigControlProxy(values, nameOpt)
//    ctl.expand
//  }
//}