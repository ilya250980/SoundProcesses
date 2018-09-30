/*
 *  BufferGen.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.proc.graph.BufferGen.Command
import de.sciss.synth.ugen.ControlProxy

object BufferGen {
  // alias these
  type Command                              = message.BufferGen.Command
  val  Cheby: message.BufferGen.Cheby.type  = message.BufferGen.Cheby
  type Cheby                                = message.BufferGen.Cheby
  val  Sine1: message.BufferGen.Sine1.type  = message.BufferGen.Sine1
  type Sine1                                = message.BufferGen.Sine1
  val  Sine2: message.BufferGen.Sine2.type  = message.BufferGen.Sine2
  type Sine2                                = message.BufferGen.Sine2
  val  Sine3: message.BufferGen.Sine3.type  = message.BufferGen.Sine3
  type Sine3                                = message.BufferGen.Sine3

  def controlName(id: Int): String = s"$$buf_gen$id"

  def sine1(partials: Seq[Float], normalize: Boolean = true, wavetable: Boolean = true,
            clear: Boolean = true, numFrames: GE, numChannels: GE = 1): BufferGen = {
    val cmd = Sine1(
      partials = partials, normalize = normalize, wavetable = wavetable, clear = clear)
    BufferGen(cmd, numFrames = numFrames, numChannels = numChannels)
  }

  def sine2(partials: Seq[(Float, Float)], normalize: Boolean = true, wavetable: Boolean = true,
            clear: Boolean = true, numFrames: GE, numChannels: GE = 1): BufferGen = {
    val cmd = Sine2(
      partials = partials, normalize = normalize, wavetable = wavetable, clear = clear)
    BufferGen(cmd, numFrames = numFrames, numChannels = numChannels)
  }

  def sine3(partials: Seq[(Float, Float, Float)], normalize: Boolean = true, wavetable: Boolean = true,
            clear: Boolean = true, numFrames: GE, numChannels: GE = 1): BufferGen = {
    val cmd = Sine3(
      partials = partials, normalize = normalize, wavetable = wavetable, clear = clear)
    BufferGen(cmd, numFrames = numFrames, numChannels = numChannels)
  }

  def cheby(amps: Seq[Float], normalize: Boolean = true, wavetable: Boolean = true,
            clear: Boolean = true, numFrames: GE, numChannels: GE = 1): BufferGen = {
    val cmd = Cheby(
      amps = amps, normalize = normalize, wavetable = wavetable, clear = clear)
    BufferGen(cmd, numFrames = numFrames, numChannels = numChannels)
  }
}

/** Creates a buffer filled by a special buffer-generation (`/b_gen`) function,
  * for example to create a wave-shaper table.
  *
  * @param cmd            the buffer generator command
  * @param numFrames      the number of frames for the buffer.
  *                       Must be resolvable at graph expansion time.
  * @param numChannels    the number of channels for the buffer (defaults to `1`).
  *                       Must be resolvable at graph expansion time.
  */
final case class BufferGen(cmd: Command, numFrames: GE, numChannels: GE = 1) extends GE.Lazy with ScalarRated {
  import BufferOut.{canResolve, resolveFloat}

  private def fail(arg: String, detail: String): Nothing =
    throw new IllegalArgumentException(s"BufferGen.$arg cannot be resolved at initialization time: $detail")

  canResolve(numFrames  ).left.foreach(fail("numFrames"  , _))
  canResolve(numChannels).left.foreach(fail("numChannels", _))

  protected def makeUGens: UGenInLike = {
    val b             = UGenGraphBuilder.get
    val numFramesI    = resolveFloat(numFrames  , b).fold[Float](fail("numFrames"  , _), identity).toInt
    val numChannelsI  = resolveFloat(numChannels, b).fold[Float](fail("numChannels", _), identity).toInt

    val value         = b.requestInput(Input.BufferGen(cmd = cmd, numFrames = numFramesI, numChannels = numChannelsI))
    val ctlName       = BufferGen.controlName(value.id)
    val ctl           = ControlProxy(rate, Vector(0f), Some(ctlName))
    ctl.expand
  }
}
