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
  private val CtlSingleZero = ControlValues(Vector(0f))

  final class Factory(val `this`: String) extends AnyVal { me =>
    import me.{`this` => name}

    def ir: Attribute = ir(CtlSingleZero)
    def ir(values: ControlValues): Attribute = Attribute.ir(key = name, default = values)

    def kr: Attribute = kr(CtlSingleZero)
    def kr(values: ControlValues): Attribute = Attribute.kr(key = name, default = values)

//    def tr: TrigControlProxy = tr(ControlValues.singleZero)
//    def tr(values: ControlValues): TrigControlProxy  = TrigControlProxy     (values.seq, Some(name))

    def ar: Attribute = ar(CtlSingleZero)
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

  def ir(key: String, default: ControlValues = 0.0, fixed: Boolean = false): Attribute =
    new Attribute(scalar , key, default.seq, fixed = fixed)

  def kr(key: String, default: ControlValues = 0.0, fixed: Boolean = false): Attribute =
    new Attribute(control, key, default.seq, fixed = fixed)

  def ar(key: String, default: ControlValues = 0.0, fixed: Boolean = false): Attribute =
    new Attribute(audio  , key, default.seq, fixed = fixed)
}
final case class Attribute(rate: Rate, key: String, default: Vec[Float], fixed: Boolean)
  extends GE.Lazy {

  def makeUGens: UGenInLike = {
    val b         = UGenGraphBuilder.get
    val defChans  = default.size
    val inValue   = b.requestInput(Input.Scalar(
      name                = key,
      requiredNumChannels = if (fixed) defChans else -1,
      defaultNumChannels  = defChans))
    val numCh   = inValue.numChannels
    val ctlName = Attribute.controlName(key)
//    val values  = default.tabulate(numCh)
    val values  = Vector.tabulate(numCh)(idx => default(idx % defChans))
    val nameOpt = Some(ctlName)
    val ctl     = if (rate == audio)
      AudioControlProxy(values, nameOpt)
    else
      ControlProxy(rate, values, nameOpt)
    ctl.expand
  }
}