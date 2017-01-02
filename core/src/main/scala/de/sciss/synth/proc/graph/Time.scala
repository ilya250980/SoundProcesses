/*
 *  Time.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.synth.Ops.stringToControl

object Time {
  private[proc] final val key = "$time"

  def ir: GE = Time()
}
/** Absolute time on the canvas, in seconds. */
final case class Time() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Time.key.ir
}

object Offset {
  private[proc] final val key = "$off"

  def ir: GE = Offset()
}
/** Start time offset within the proc, in seconds. Will be zero if proc is started from the beginning. */
final case class Offset() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Offset.key.ir
}

object Duration {
  private[proc] final val key = "$dur"

  def ir: GE = Duration()
}

/** Total duration of proc in seconds. If proc was started midway through, this is still its total
  * length. To gather for how long it's going to play, use `Duration() - Offset()`.
  */
final case class Duration() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Duration.key.ir(inf)
}
