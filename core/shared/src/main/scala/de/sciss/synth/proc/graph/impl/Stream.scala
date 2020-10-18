/*
 *  Stream.scala
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

package de.sciss.synth.proc.graph.impl

import de.sciss.lucre.synth.Server
import de.sciss.synth
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.{UGen, UGenInLike, control}
import de.sciss.synth.proc.UGenGraphBuilder
import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.{UGenInGroup, UGenOutProxy}

object Stream {
  def controlName(key: String, idx: Int): String = s"$$str${idx}_$key"

  trait Info extends Stream {
    protected def maxSpeed  = 0.0
    protected def interp    = 0
  }

  /** Creates a `Done` trigger UGen for a `DiskIn` or `VDiskIn` */
  def mkDoneUGen(in: Stream): UGenInLike = {
    // these are the `flatOutputs` we may discover:
    //
    //     println(in.expand.flatOutputs)
    //
    //  mono file, no mce:
    //  Vector(
    //    BinaryOpUGen.ar(DiskIn.ar(_, _), _)
    //  )
    //
    //  mono file, mce for loop:
    //    Vector(
    //    BinaryOpUGen.ar(DiskIn.ar(_, _), _),
    //    BinaryOpUGen.ar(DiskIn.ar(_, _), _)
    //  )
    //
    //  stereo file, no mce:
    //  Vector(
    //    BinaryOpUGen.ar(DiskIn.ar(_, _).\(0), _),
    //    BinaryOpUGen.ar(DiskIn.ar(_, _).\(1), _)
    //  )
    //
    //  stereo file, mce for loop:
    //
    //  Vector(
    //    BinaryOpUGen.ar(DiskIn.ar(_, _).\(0), _),
    //    BinaryOpUGen.ar(DiskIn.ar(_, _).\(1), _),
    //    BinaryOpUGen.ar(DiskIn.ar(_, _).\(0), _),
    //    BinaryOpUGen.ar(DiskIn.ar(_, _).\(1), _)
    //  )
    //
    // Note that `UGen.MultiOutImpl` is not a case class,
    // wo we just have to collect the DiskIn UGens in a distinct set,
    // and produce `Done` UGens for them.
    //
    // We use a "dumb" pattern matching here and `.head`, so that if we
    // actually change the implementation or the UGen has zero outputs,
    // we get a runtime error instead of silently failing to correctly produce the done UGen.

    val doneIn = in.expand.flatOutputs.map {
      case bin: UGen.SingleOut if bin.name == "BinaryOpUGen" =>
        val diskIn = bin.inputs.head match {
          case u: UGen.MultiOut   => u
          case UGenOutProxy(u, _) => u
        }
        assert (diskIn.name == "DiskIn" || diskIn.name == "VDiskIn")
        diskIn

    } .distinct

    //      println(doneIn)

    val seq = doneIn.map { in =>
      UGen.SingleOut("Done", control, Vector(in.outputs.head), hasSideEffect = true).expand
    }

    if (seq.size == 1) seq.head else UGenInGroup(seq)
    //      Impulse.kr(0)
  }
}
trait Stream extends synth.GE.Lazy {
  protected def makeUGen(server: Server, numChannels: Int, sampleRate: Double, idx: Int, buf: synth.GE,
                         gain: synth.GE): UGenInLike

  protected def key: String

  protected def maxSpeed: Double
  protected def interp  : Int

  def makeUGens: UGenInLike = {
    val b = UGenGraphBuilder.get
    import synth._

    val interp1       = if (interp == 4) -1 else interp // see Input.Stream.Spec ('native')
    val spec          = Input.Stream.Spec(maxSpeed = maxSpeed, interp = interp1)
    val info          = b.requestInput(Input.Stream(key, spec))
    val idx           = if (spec.isEmpty) 0 else info.specs.size - 1
    // val (numCh, idx)  = b.addStreamIn(key, info)
    val ctlName       = Stream.controlName(key, idx)
    val ctl           = ctlName.ir(Seq(0, 0))
    val buf           = ctl out 0
    val gain          = ctl out 1
    makeUGen(server = b.server, numChannels = info.numChannels, sampleRate = info.sampleRate,
      idx = idx, buf = buf, gain = gain)
  }
}