/*
 *  AudioArtifactScalarWriter.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.synth.{Buffer, ControlBus, Resource, Synth, Txn}
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.proc.AudioCue
import de.sciss.synth.{ControlSet, SynthGraph, addToHead}

object AudioArtifactScalarWriter {
  def apply(bus: ControlBus, audioVal: AudioCue)(implicit tx: Txn): Resource = {
    val numChannels = audioVal.spec.numChannels
    val sg  = SynthGraph {
      import de.sciss.synth._
      import ugen._
      val buf   = "buf".ir
      val out   = "out".kr
      val amp   = "amp".kr(1f)
      val sig0  = BufRd.kr(numChannels, buf = buf, loop = 0, interp = 1)
      val sig   = sig0 * amp
      // Line.kr(start = 0, end = 0, dur = dur, doneAction = freeSelf)
      Out.kr(out, sig)            // note: doesn't work: `Out.ir(...)`
      FreeSelf.kr(Impulse.kr(0))  // note: doesn't work: `DC.kr(1)`
    }
    val synth = Synth(bus.server, sg, nameHint = Some("audio-artifact"))
    val res = new Impl(synth, bus, audioVal)
    res.play()
    res
  }

  private final class Impl(synth: Synth, bus: ControlBus, audioVal: AudioCue)
    extends Resource.Proxy {

    protected def resourcePeer: Resource = synth

    def play()(implicit tx: Txn): Unit = {
      val file      = audioVal.artifact
      val path      = file.getAbsolutePath
      val target    = server.defaultGroup // XXX
      val rb        = Buffer(server)(numFrames = 1, numChannels = bus.numChannels)
      rb.read(path, fileStartFrame = audioVal.fileOffset, numFrames = 1)
      val args      = List[ControlSet]("buf" -> rb.id, "amp" -> audioVal.gain)

      // val rs = rd.play(target = target, args = args, buffers = rb :: Nil)
      synth.play(target = target, args = args, addAction = addToHead, dependencies = rb :: Nil)

      synth.onEndTxn { implicit tx =>
        rb.dispose()
      }

      synth.write(bus -> "out")
    }
  }
}