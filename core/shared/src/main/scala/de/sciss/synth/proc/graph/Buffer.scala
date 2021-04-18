/*
 *  Buffer.scala
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

import de.sciss.proc.UGenGraphBuilder
import de.sciss.proc.UGenGraphBuilder.Input
import de.sciss.synth.UGenSource.{ProductType, RefMapIn}
import de.sciss.synth.ugen.ControlProxy
import de.sciss.synth.{GE, Rate, UGenInLike, control, scalar}

import scala.collection.immutable.{IndexedSeq => Vec}

/** An element referring to a random access buffer provided through an attribute.
  * The attribute will typically be an audio grapheme.
  */
object Buffer extends ProductType[Buffer] {
  override final val typeId = 603

  def ir(key: String): Buffer = new Buffer(scalar , key)
  def kr(key: String): Buffer = new Buffer(control, key)

  /** Convenience alias for `kr` */
  def apply(key: String): Buffer = kr(key)

  /* private[proc] */ def controlName(key: String): String = s"$$buf_$key"

  override def read(in: RefMapIn, prefix: String, arity: Int): Buffer = {
    require (arity == 2)
    val _rate     = in.readRate()
    val _key      = in.readString()
    new Buffer(_rate, _key)
  }
}

/** An element referring to a random access buffer provided through an attribute.
  * The attribute will typically be an audio grapheme.
  *
  * @param key  the attribute key.
  */
final case class Buffer(rate: Rate, key: String) extends GE.Lazy {
  protected def makeUGens: UGenInLike = {
    val b       = UGenGraphBuilder.get
    b.requestInput(Input.Buffer(key))
    val ctlName = Buffer.controlName(key)
    val ctl     = ControlProxy(rate, Vec(0f), Some(ctlName))
    ctl.expand
  }
}