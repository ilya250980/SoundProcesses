/*
 *  Action.scala
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

object Action extends ProductType[Action] {
  override final val typeId = 600

  override def read(in: RefMapIn, prefix: String, arity: Int): Action = {
    require (arity == 2)
    val _trig = in.readGE()
    val _key  = in.readString()
    new Action(_trig, _key)
  }
}
/** A graph element that executes an action upon receiving a trigger.
  *
  * @param trig   the trigger input signal
  * @param key    a key into the process' attribute map. the value peer stored
  *               at that location should be of type `proc.Action`
  */
final case class Action(trig: GE, key: String) extends Lazy.Expander[Unit] with HasSideEffect {
  protected def makeUGens: Unit = {
    val b = UGenGraphBuilder.get
    b.requestInput(Input.Action(key))
    impl.ActionResponder.makeUGen(trig, None, key)
  }
}

object Reaction extends ProductType[Reaction] {
  override final val typeId = 601

  override def read(in: RefMapIn, prefix: String, arity: Int): Reaction = {
    require (arity == 3)
    val _trig = in.readGE()
    val _in   = in.readGE()
    val _key  = in.readString()
    new Reaction(_trig, _in, _key)
  }
}
/** A graph element that executes an action upon receiving a trigger,
  * sampling the values at that moment and making them available
  * in the action through the `values` method.
  *
  * @param trig   the trigger input signal
  * @param in     the input signal to sample and pass on to the action
  * @param key    a key into the process' attribute map. the value peer stored
  *               at that location should be of type `proc.Action`
  */
final case class Reaction(trig: GE, in: GE, key: String) extends Lazy.Expander[Unit] with HasSideEffect {
  protected def makeUGens: Unit = {
    val b = UGenGraphBuilder.get
    b.requestInput(Input.Action(key))
    impl.ActionResponder.makeUGen(trig, Some(Flatten(in)), key)
  }
}
