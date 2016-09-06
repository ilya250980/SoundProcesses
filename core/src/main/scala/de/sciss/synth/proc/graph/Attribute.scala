/*
 *  Attribute.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
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
import scala.language.implicitConversions

object Attribute {
  // private val CtlSingleZero = ControlValues(Vector(0f))

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

//    def tr: TrigControlProxy = tr(ControlValues.singleZero)
//    def tr(values: ControlValues): TrigControlProxy  = TrigControlProxy     (values.seq, Some(name))

    /** Creates an attribute without defaults (attribute must be present). */
    def ar: Attribute = Attribute.ar(key = name)
    /** Creates an attribute with defaults (attribute may be absent). */
    def ar(values: ControlValues): Attribute = Attribute.ar(key = name, default = values)
  }

//  object Default {
//    implicit def scalar(in:     Double ): Default = Scalar(in)
//    implicit def vector(in: Vec[Double]): Default = Vector(in)
//  }
//  /** Magnet pattern */
//  sealed trait Default extends Product {
//    def numChannels: Int
//    def tabulate(n: Int): Vec[Float]
//  }
//  final case class Scalar(value: Double) extends Default {
//    def numChannels = 1
//
//    def tabulate(n: Int): Vec[Float] = scala.Vector.fill(n)(value.toFloat)
//
//    // serialization!
//    override def productPrefix: String = "Attribute$Scalar"
//  }
//
//  final case class Vector(values: Vec[Double]) extends Default {
//    def numChannels: Int = values.size
//
//    def tabulate(n: Int): Vec[Float] = scala.Vector.tabulate(n)(idx => values(idx % values.size).toFloat)
//
//    // serialization!
//    override def productPrefix: String = "Attribute$Vector"
//  }

  /* private[proc] */ def controlName(key: String): String = s"$$at_$key"

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

  private def mk(rate: Rate, key: String, default: ControlValues, fixed: Boolean): Attribute =
    new Attribute(scalar , key, Some(default.seq), fixed = if (fixed) default.seq.size else -1)
}
final case class Attribute(rate: Rate, key: String, default: Option[Vec[Float]], fixed: Int)
  extends GE.Lazy {

  def makeUGens: UGenInLike = {
    val b         = UGenGraphBuilder.get
    val defChans  = default.fold(-1)(_.size)
    val inValue   = b.requestInput(Input.Scalar(
      name                = key,
      requiredNumChannels = fixed,
      defaultNumChannels  = defChans))
    val numCh   = inValue.numChannels
    val ctlName = Attribute.controlName(key)
    val values  = default.fold(Vector.fill(numCh)(0f))(df => Vector.tabulate(numCh)(idx => df(idx % defChans)))
    val nameOpt = Some(ctlName)
    val ctl     = if (rate == audio)
      AudioControlProxy(values, nameOpt)
    else
      ControlProxy(rate, values, nameOpt)
    ctl.expand
  }
}