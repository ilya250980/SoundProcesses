/*
 *  Action.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.graph

import de.sciss.synth.proc.UGenGraphBuilder
import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.Flatten
import de.sciss.synth.{GE, HasSideEffect, Lazy}

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
