/*
 *  stream.scala
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

import de.sciss.lucre.synth.Server
import de.sciss.proc.UGenGraphBuilder
import de.sciss.proc.UGenGraphBuilder.Input
import de.sciss.proc.impl.StreamBuffer
import de.sciss.synth
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.UGenSource.{ProductReader, RefMapIn}
import de.sciss.synth.proc.graph.impl.Stream
import de.sciss.synth.ugen.Constant
import de.sciss.synth.{ControlRated, GE, IsIndividual, Rate, UGenInLike, WritesBuffer, audio, control, scalar, ugen}

// ---- ugen wrappers ----

object DiskIn extends ProductReader[DiskIn] {
  /** A SoundProcesses aware variant of `DiskIn`. It takes its streaming buffer input from
    * an attribute with the given `key`. Like the original `DiskIn` UGen, this does not perform
    * sample-rate-conversion if server rate and file rate diverge.
    *
    * The `done` method can be used to determine when the file has been played completely.
    *
    * @param key      key into the containing object's attribute map, where an `AudioCue` is to be found.
    * @param loop     whether to loop (greater than zero) or not (zero, the default).
    */
  def ar(key: String, loop: synth.GE = 0): DiskIn = apply(audio, key = key, loop = loop)

  object Done extends ProductReader[Done] {
    override def read(in: RefMapIn, key: String, arity: Int): Done = {
      require (arity == 1)
      val _in = in.readProductT[DiskIn]()
      new Done(_in)
    }
  }
  final case class Done(in: DiskIn) extends GE.Lazy with ControlRated {
    override def productPrefix = s"DiskIn$$Done"  // for serialization

    protected def makeUGens: UGenInLike = Stream.mkDoneUGen(in)
  }

  override def read(in: RefMapIn, prefix: String, arity: Int): DiskIn = {
    require (arity == 3)
    val _rate   = in.readRate()
    val _key    = in.readString()
    val _loop   = in.readGE()
    new DiskIn(_rate, _key, _loop)
  }
}
final case class DiskIn(rate: Rate, key: String, loop: synth.GE)
  extends Stream with IsIndividual {

  protected def maxSpeed: Double  = 1.0
  protected def interp  : Int     = -1

  /** A trigger signal for when the UGen has gone through the entire file.
    * Note that this is only signalled if the UGen is not looping.
    */
  def done: GE = DiskIn.Done(this)

  protected def makeUGen(server: Server, numChannels: Int, sampleRate: Double, idx: Int,
                         buf: synth.GE, gain: synth.GE): UGenInLike = {
    import de.sciss.synth.geOps
    ugen.DiskIn(rate, numChannels = numChannels, buf = buf, loop = loop) * gain
  }
}

object VDiskIn extends ProductReader[VDiskIn] {
  /** A SoundProcesses aware variant of `VDiskIn`. It takes its streaming buffer input from
    * an attribute with the given `key`. Default values provide automatic sample-rate-conversion
    * to match the audio server.
    *
    * @param key      key into the containing object's attribute map, where an `AudioCue` is to be found.
    * @param speed    speed factor as in `ugen.VDiskIn`. If a negative constant value is given,
    *                 the actual factor is `BufRateScale.kr * -speed`, thus `-1` indicates playback
    *                 at correct sample rate.
    * @param interp   same as in `ugen.VDiskIn`. Additionally, a value of `-1` indicates that
    *                 interpolation should be chosen according to `speed`. This is useful in conjunction
    *                 with negative speed values where interpolation might depend on actual SRC.
    * @param maxSpeed maximum expected speed, which will be used in consideration of the buffer size needed.
    *                 if zero (default), and `speed` is a constant, this will be aligned with `speed`.
    */
  def ar(key: String, speed: synth.GE = -1, loop: synth.GE = 0, interp: Int = -1, maxSpeed: Double = 0.0): VDiskIn = {
    // XXX TODO: match against UserValue ?
    val maxSpeed1: Double = speed match {
      case Constant(c)    => math.abs(c)
      case _              => maxSpeed
    }
    apply(audio, key = key, speed = speed, loop = loop, interp = interp, maxSpeed = maxSpeed1)
  }

  object Done extends ProductReader[Done] {
    override def read(in: RefMapIn, key: String, arity: Int): Done = {
      require (arity == 1)
      val _in = in.readProductT[VDiskIn]()
      new Done(_in)
    }
  }
  final case class Done(in: VDiskIn) extends GE.Lazy with ControlRated {
    override def productPrefix = s"VDiskIn$$Done"  // for serialization

    protected def makeUGens: UGenInLike = Stream.mkDoneUGen(in)
  }

  override def read(in: RefMapIn, prefix: String, arity: Int): VDiskIn = {
    require (arity == 6)
    val _rate     = in.readRate()
    val _key      = in.readString()
    val _speed    = in.readGE()
    val _loop     = in.readGE()
    val _interp   = in.readInt()
    val _maxSpeed = in.readDouble()
    new VDiskIn(_rate, _key, _speed, _loop, _interp, _maxSpeed)
  }
}

/** A SoundProcesses aware variant of `VDiskIn`. It takes its streaming buffer input from
  * an attribute with the given `key`. Default values provide automatic sample-rate-conversion
  * to match the audio server.
  *
  * @param key      key into the containing object's attribute map, where an `AudioCue` is to be found.
  * @param speed    speed factor as in `ugen.VDiskIn`. If a negative constant value is given,
  *                 the actual factor is `BufRateScale.kr * -speed`, thus `-1` indicates playback
  *                 at correct sample rate.
  * @param interp   same as in `ugen.VDiskIn`. Additionally, a value of `-1` indicates that
  *                 interpolation should be chosen according to `speed`. This is useful in conjunction
  *                 with negative speed values where interpolation might depend on actual SRC.
  * @param maxSpeed maximum expected speed, which will be used in consideration of the buffer size needed.
  *                 if zero (default), and `speed` is a constant, this will be aligned with `speed`.
  */
final case class VDiskIn(rate: Rate, key: String, speed: synth.GE, loop: synth.GE, interp: Int, maxSpeed: Double)
  extends Stream with IsIndividual {

  if (interp != -1 && interp != 1 && interp != 2 && interp != 4) sys.error(s"Unsupported interpolation: $interp")

  // VDiskIn uses cubic interpolation. Thus provide native streaming if that interpolation
  // is chosen; otherwise use the `StreamBuffer` functionality.

  /** A trigger signal for when the UGen has gone through the entire file.
    * This requires that interpolation is '''not 2''' (linear) which is currently not supported to produce
    * `done` information.
    *
    * Note that this is only signalled if the UGen is not looping.
    */
  def done: GE = VDiskIn.Done(this)

  protected def makeUGen(server: Server, numChannels: Int, sampleRate: Double, idx: Int,
                         buf: synth.GE, gain: synth.GE): UGenInLike = {
    val speed1: GE = speed match {
      case Constant(v) if v < 0 => sampleRate / server.sampleRate * -v
      case other                => other
    }
    val constSpeed = speed1 == Constant.C1
    // if we know that speed is exactly one, we can go for no-interpolation
    val interp1 = if (interp != -1) interp else if (constSpeed) 1 else 4
    val reader = if (interp1 == 4) {
      ugen.VDiskIn(rate, numChannels = numChannels, buf = buf, speed = speed1, loop = loop)
    } else if (interp1 == 1 && constSpeed) {
      ugen.DiskIn(rate, numChannels = numChannels, buf = buf, loop = loop)
    } else {
      StreamBuffer.makeUGen(key = key, idx = idx, buf = buf, numChannels = numChannels, speed = speed1, interp = interp1)
    }
    import de.sciss.synth.geOps
    reader * gain
  }
}

object DiskOut extends ProductReader[DiskOut] {
  /* private[proc] */ def controlName(key: String): String = s"$$disk_$key"

  def ar(key: String, in: GE): DiskOut = apply(audio, key = key, in = in)

  override def read(in: RefMapIn, prefix: String, arity: Int): DiskOut = {
    require (arity == 3)
    val _rate     = in.readRate()
    val _key      = in.readString()
    val _in       = in.readGE()
    new DiskOut(_rate, _key, _in)
  }
}

/** A graph element that creates a `DiskOut` writing to a file
  * designated by an object attribute with a given `key` and the
  * value being an `Artifact`.
  *
  * The file-type is determined by this artifact. For example, if the
  * artifact's path ends in `".aif"`, the AIFF format will used, if
  * the path ends in `".w64"`, then Wave64 will be used. The default is AIFF.
  * The sample format is currently always Float-32.
  *
  * @param key  the key into the enclosing object's attribute map, pointing to an `Artifact`
  * @param in   the signal to write
  */
final case class DiskOut(rate: Rate, key: String, in: GE)
  extends synth.GE.Lazy with WritesBuffer {

  protected def makeUGens: UGenInLike = {
    import synth._
    val b         = UGenGraphBuilder.get
    val ins       = in.expand.flatOutputs
    b.requestInput(Input.DiskOut(key, numChannels = ins.size))
    val ctlName   = DiskOut.controlName(key)
    val buf       = ctlName.ir
    ugen.DiskOut(rate, buf = buf, in = ins)
  }
}

object BufChannels extends ProductReader[BufChannels] {
  def ir(key: String): BufChannels = apply(scalar , key = key)
  def kr(key: String): BufChannels = apply(control, key = key)

  override def read(in: RefMapIn, prefix: String, arity: Int): BufChannels = {
    require (arity == 2)
    val _rate     = in.readRate()
    val _key      = in.readString()
    new BufChannels(_rate, _key)
  }
}
final case class BufChannels(rate: Rate, key: String) extends Stream.Info {
  protected def makeUGen(server: Server, numChannels: Int, sampleRate: Double, idx: Int,
                         buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufChannels(rate, buf) // or just Constant(numChannels), ha?
}

object BufRateScale extends ProductReader[BufRateScale] {
  def ir(key: String): BufRateScale = apply(scalar , key = key)
  def kr(key: String): BufRateScale = apply(control, key = key)

  override def read(in: RefMapIn, prefix: String, arity: Int): BufRateScale = {
    require (arity == 2)
    val _rate     = in.readRate()
    val _key      = in.readString()
    new BufRateScale(_rate, _key)
  }
}
final case class BufRateScale(rate: Rate, key: String) extends Stream.Info {
  protected def makeUGen(server: Server, numChannels: Int, sampleRate: Double, idx: Int,
                         buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufRateScale(rate, buf)
}

object BufSampleRate extends ProductReader[BufSampleRate] {
  def ir(key: String): BufSampleRate = apply(scalar , key = key)
  def kr(key: String): BufSampleRate = apply(control, key = key)

  override def read(in: RefMapIn, prefix: String, arity: Int): BufSampleRate = {
    require (arity == 2)
    val _rate     = in.readRate()
    val _key      = in.readString()
    new BufSampleRate(_rate, _key)
  }
}
final case class BufSampleRate(rate: Rate, key: String) extends Stream.Info {
  protected def makeUGen(server: Server, numChannels: Int, sampleRate: Double, idx: Int,
                         buf: synth.GE, gain: synth.GE): UGenInLike =
    ugen.BufSampleRate(rate, buf)
}