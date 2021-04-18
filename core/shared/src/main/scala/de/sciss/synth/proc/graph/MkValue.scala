/*
 *  MkValue.scala
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
import de.sciss.synth.ugen.Flatten
import de.sciss.synth.{GE, HasSideEffect, Lazy}

object MkValue extends ProductType[MkValue] {
  override final val typeId = 610

  override def read(in: RefMapIn, prefix: String, arity: Int): MkValue = {
    require (arity == 3)
    val _key  = in.readString()
    val _trig = in.readGE()
    val _in   = in.readGE()
    new MkValue(_key, _trig, _in)
  }
}
/** A graph element that writes values to a caller variable upon receiving a trigger,
  * sampling the values at that moment. If the variable is absent, the element
  * is simply a no-op. The variable type should be `Double` or `Seq[Double]`.
  *
  * @param key    a key into the process' attribute map. the value peer stored
  *               at that location should be of type `expr.Var`
  * @param trig   the trigger input signal
  * @param in     the input signal to sample and write into the variable
  */
final case class MkValue(key: String, trig: GE, in: GE) extends Lazy.Expander[Unit] with HasSideEffect {
  protected def makeUGens: Unit = {
    val b = UGenGraphBuilder.get
    val v = b.requestInput(Input.MkValue(key))
    if (v.defined) impl.MkValueResponder.makeUGen(trig, Flatten(in), key)
  }
}
