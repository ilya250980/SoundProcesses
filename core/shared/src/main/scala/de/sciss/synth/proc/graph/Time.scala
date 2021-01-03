/*
 *  Time.scala
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

import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.{GE, ScalarRated, UGenInLike, inf}

object Time {
  private[sciss] final val key = "$time"

  def ir: GE = Time()
}
/** Absolute time on the canvas, in seconds. */
final case class Time() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Time.key.ir
}

object Offset {
  private[sciss] final val key = "$off"

  def ir: GE = Offset()
}
/** Start time offset within the proc, in seconds. Will be zero if proc is started from the beginning. */
final case class Offset() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Offset.key.ir
}

object Duration {
  private[sciss] final val key = "$dur"

  def ir: GE = Duration()
}

/** Total duration of proc in seconds. If proc was started midway through, this is still its total
  * length. To gather for how long it's going to play, use `Duration() - Offset()`.
  */
final case class Duration() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Duration.key.ir(inf)
}
