/*
 *  BufferGen.scala
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
import de.sciss.synth.proc.graph.BufferGen.Command
import de.sciss.synth.ugen.ControlProxy
import de.sciss.synth.{GE, ScalarRated, UGenInLike, message}

object BufferGen extends ProductType[BufferGen] {
  override final val typeId = 604

  object Command extends ProductType[Command] { // XXX TODo why was this made `ProductType`? remove in major version
    override final val typeId = 605

    override def read(in: RefMapIn, prefix: String, arity: Int): Command = ???
  }
  // alias these
  type Command                              = message.BufferGen.Command
  val  Cheby: message.BufferGen.Cheby.type  = message.BufferGen.Cheby
  type Cheby                                = message.BufferGen.Cheby
  val  Copy: message.BufferGen.Copy.type    = message.BufferGen.Copy
  type Copy                                 = message.BufferGen.Copy
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

  override def read(in: RefMapIn, prefix: String, arity: Int): BufferGen = {
    ??? // register Command here as well!
    require (arity == 3)
    val _cmd          = in.readProductT[Command]()
    val _numFrames    = in.readGE()
    val _numChannels  = in.readGE()
    new BufferGen(_cmd, _numFrames, _numChannels)
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
