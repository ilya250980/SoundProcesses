/*
 *  StopSelf.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.synth.{GE, Lazy, ugen}

object StopSelf {
  final val replyName = "/$stop"
}

/** A UGen that when triggers stops the encompassing synth.
  * As opposed to `FreeSelf`, this triggers a disposal action on
  * the client side, possibly synchronising the disposal of
  * associated resources.
  *
  * @param trig   the trigger signal that invokes the disposal
  * @param pause  if non-zero, immediately pauses the synth
  *               when the trigger is received. This can be useful
  *               because the client may take a moment to actually
  *               dispose the synth.
  */
final case class StopSelf(trig: GE, pause: GE = 1) extends Lazy.Expander[Unit] {
  protected def makeUGens: Unit = {
    import ugen._
    PauseSelf.kr(trig & pause)
    val b = UGenGraphBuilder.get
    b.requestInput(Input.StopSelf)
    SendReply(trig.rate, trig, values = 0, msgName = StopSelf.replyName)
  }
}
