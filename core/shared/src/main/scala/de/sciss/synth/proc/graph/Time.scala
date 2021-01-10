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
import de.sciss.synth.UGenSource.{ProductReader, RefMapIn}
import de.sciss.synth.{GE, ScalarRated, UGenInLike, inf}

object Time extends ProductReader[Time] {
  private[sciss] final val key = "$time"

  def ir: GE = Time()

  override def read(in: RefMapIn, prefix: String, arity: Int): Time = {
    require (arity == 0)
    new Time()
  }
}
/** Absolute time on the canvas, in seconds. */
final case class Time() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Time.key.ir
}

object Offset extends ProductReader[Offset] {
  private[sciss] final val key = "$off"

  def ir: GE = Offset()

  override def read(in: RefMapIn, prefix: String, arity: Int): Offset = {
    require (arity == 0)
    new Offset()
  }
}
/** Start time offset within the proc, in seconds. Will be zero if proc is started from the beginning. */
final case class Offset() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Offset.key.ir
}

object Duration extends ProductReader[Duration] {
  private[sciss] final val key = "$dur"

  def ir: GE = Duration()

  override def read(in: RefMapIn, prefix: String, arity: Int): Duration = {
    require (arity == 0)
    new Duration()
  }
}

/** Total duration of proc in seconds. If proc was started midway through, this is still its total
  * length. To gather for how long it's going to play, use `Duration() - Offset()`.
  */
final case class Duration() extends GE.Lazy with ScalarRated {
  protected def makeUGens: UGenInLike = Duration.key.ir(inf)
}
