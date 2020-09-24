package de.sciss
package synth
package proc

import de.sciss.serial.{DataInput, DataOutput}
import de.sciss.synth.Curve.cubed
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.ugen._
import org.scalatest.funspec.AnyFunSpec

/*
  To run only this suite:

  testOnly de.sciss.synth.proc.SynthGraphSerializationSpec

 */
class SynthGraphSerializationSpec extends AnyFunSpec {
  var dfs = Map.empty[String, SynthGraph]

  dfs += "AnalogBubbles" -> SynthGraph {
    val f1  = "freq1".kr(0.4)
    val f2  = "freq2".kr(8.0)
    val d   = "detune".kr(0.90375)
    val f   = LFSaw.ar(f1).mulAdd(24, LFSaw.ar(Seq(f2, f2 * d)).mulAdd(3, 80)).midiCps // glissando function
    val res = CombN.ar(SinOsc.ar(f) * 0.04, 0.2, 0.2, 4) // echoing sine wave
    Out.ar(0, res)
  }

  //////////////////////////////////////////////////////////////////////////

  // LFO modulation of Pulse waves and resonant filters
  dfs += "LFO mod" -> SynthGraph {
    val res = CombL.ar(
      RLPF.ar(LFPulse.ar(FSinOsc.kr(0.05).mulAdd(80, 160), 0, 0.4) * 0.05,
        FSinOsc.kr(Seq(0.6, 0.7)).mulAdd(3600, 4000), 0.2),
      0.3, Seq(0.2, 0.25), 2)
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // scratchy
  dfs += "Scratchy" -> SynthGraph {
    val res = RHPF.ar((BrownNoise.ar(Seq(0.5, 0.5)) - 0.49).max(0) * 20, 5000, 1)
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // harmonic swimming
  dfs += "HarmonicSwim" -> SynthGraph {
    val f       = 50 // fundamental frequency
    val p       = 20 // number of partials per channel
    val offset  = Line.kr(0, -0.02, 60, doneAction = freeSelf) // causes sound to separate and fade
    val res     = Mix.tabulate(p) { i =>
      val sig = FSinOsc.ar(f * (i + 1)) * // freq of partial
        LFNoise1.kr(
          Seq(Rand(2, 10), Rand(2, 10))).mulAdd(// amplitude rate
          0.02, // amplitude scale
          offset // amplitude offset
        ).max(0) // clip negative amplitudes to zero
      sig
    }
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // Klank - bank of resonators excited by impulses
  dfs += "Klank" -> SynthGraph {
    val p = 15 // number of partials
    val z = // filter bank specification :
      KlangSpec.fill(p) {
        (Rand(80, 10080), // frequencies
          Rand(-1, 1), // amplitudes
          Rand(0.2, 8.2)) // ring times
      }
    val res = Pan2.ar(
      Klank.ar(z, Dust.ar(0.7) * 0.04),
      Rand(-1, 1)
    )
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // cymbalism
  dfs += "Cymbalism" -> SynthGraph {
    val p   = 15 // number of partials per channel per 'cymbal'.
    val f1  = Rand(500, 2500)
    val f2  = Rand(0, 8000)
    val res = for (_ <- 1 to 2) yield {
      val z = KlangSpec.fill(p) {
        // sine oscil bank specification :
        (f1 + Rand(0, f2), // frequencies
          1, // amplitudes
          Rand(1, 5)) // ring times
      }
      Klank.ar(z, Decay.ar(Impulse.ar(Rand(0.5, 3.5)), 0.004) * WhiteNoise.ar(0.03))
    }
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // reverberated sine percussion
  dfs += "SinePercReverb" -> SynthGraph {
    val d = 6 // number of percolators
    val c = 5 // number of comb delays
    val a = 4 // number of allpass delays

    // sine percolation sound :
    val s = Mix.fill(d) {
      Resonz.ar(Dust.ar(2.0 / d) * 50, Rand(200, 3200), 0.003)
    }

    // reverb predelay time :
    val z = DelayN.ar(s, 0.048)

    // 'c' length modulated comb delays in parallel :
    val y = Mix(CombL.ar(z, 0.1, LFNoise1.kr(Seq.fill(c)(Rand(0, 0.1))).mulAdd(0.04, 0.05), 15))

    // chain of 'a' allpass delays on each of two channels (2 times 'a' total) :
    val x = (0 until a).foldLeft[GE](y)((y, _) => AllpassN.ar(y, 0.050, Seq(Rand(0, 0.050), Rand(0, 0.050)), 1))

    // add original sound to reverb and play it :
    val res = s + (0.2 * x)
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // aleatoric quartet
  // mouse x controls density
  dfs += "AleatoricQuartet" -> SynthGraph {
    val amp = 0.07
    val density = MouseX.kr(0.01, 1) // mouse determines density of excitation

    // calculate multiply and add for excitation probability
    val dmul = density.reciprocal * 0.5 * amp
    val dadd = -dmul + amp

    val signal: GE = Mix.fill(4) {
      // mix an array of 4 instruments
      val excitation = PinkNoise.ar(
        // if amplitude is below zero it is clipped
        // density determines the probability of being above zero
        LFNoise1.kr(8).mulAdd(dmul, dadd).max(0)
      )

      val freq = Lag.kr(// lag the pitch so it glissandos between pitches
        LFNoise0.kr(// use low freq step noise as a pitch control
          Array(1, 0.5, 0.25)(// choose a frequency of pitch change
            util.Random.nextInt(3)))
          .mulAdd(
          7, // +/- 7 semitones
          IRand(36, 96) // random center note
        ).roundTo(1), // round to nearest semitone
        0.2 // gliss time
      ).midiCps // convert to hertz

      Pan2.ar(// pan each intrument
        CombL.ar(excitation, 0.02, freq.reciprocal, 3), // comb delay simulates string
        Rand(-1, 1) // random pan position
      )
    }

    // add some reverb via allpass delays
    val x   = (1 to 5).foldLeft(signal)((sig, _) => AllpassN.ar(sig, 0.05, Seq(Rand(0, 0.05), Rand(0, 0.05)), 1))
    val res = LeakDC.ar(x, 0.995) // delays build up a lot of DC, so leak it out here.
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // trigger and lagged controls
  dfs += "Trig" -> SynthGraph {
    val trig  = "trig".tr // trigger control
    //   val freq = "freq".kr(440 -> 4.0) // lag control (lag time 4 seconds)
    val freq  = "freq".kr(440.0) // lag control not yet implemented :-(
    val res   = SinOsc.ar(freq + Seq(0, 1)) * Decay2.kr(trig, 0.005, 1.0)
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // waiting for SendTrig replies
  dfs += "SendTrig" -> SynthGraph {
    SendTrig.kr(MouseButton.kr(lag = 0), MouseX.kr(lag = 0)) // warning: different arg order!
  }

  // using SendReply
  dfs += "SendReply" -> SynthGraph {
    SendReply.kr(Impulse.kr(10), Pitch.kr(In.ar(NumOutputBuses.ir, 2)).flatten)
  }

  //////////////////////////////////////////////////////////////////////////

  dfs += "mag-above" -> SynthGraph {
    val in    = WhiteNoise.ar(0.2)
    val fft   = FFT("buf".kr, in)
    val flt   = PV_MagAbove(fft, MouseX.kr(0, 10))
    val ifft  = IFFT.ar(flt) * Seq(0.5, 0.5)
    Out.ar("out".kr(0.0), ifft)
  }

  //////////////////////////////////////////////////////////////////////////

  dfs += "Demand" -> SynthGraph {
    val freq = DemandEnvGen.ar(
      Dseq(Seq(204, 400, 201, 502, 300, 200), inf),
      Drand(Seq(1.01, 0.2, 0.1, 2), inf) * MouseY.kr(0.01, 3, 1),
      cubed.id)
    val res = SinOsc.ar(freq * Seq(1, 1.01)) * 0.1
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  dfs += "OutputChans" -> SynthGraph {
    val in      = Mix(In.ar(NumOutputBuses.ir + Seq(0, 1)))
    val amp     = Amplitude.kr(in, 0.05, 0.05) * 0.3
    val pch     = Pitch.kr(in, ampThresh = 0.02, median = 7)
    val freq    = pch.freq
    val hasFreq = pch.hasFreq
    hasFreq.poll(1)
    val syn: GE = Mix(VarSaw.ar(freq * Seq(0.5, 1, 2), 0, LFNoise1.kr(0.3).mulAdd(0.1, 0.1)) * amp)
    val res = (1 to 6).foldLeft(syn) {
      (sig, _) =>
        AllpassN.ar(sig, 0.040, Seq(Rand(0, 0.040), Rand(0, 0.040)), 2)
    }
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // a synth def that has 4 partials
  dfs += "multi-con" -> SynthGraph {
    val harm  = "harm"  ir Seq(1,    2,    3,    4   )  // harmonics
    val amp   = "amp"   ir Seq(0.05, 0.05, 0.05, 0.05)  // amplitudes
    val ring  = "ring"  ir Seq(1,    1,    1,    1   )  // ring times
    val klank = Klank.ar(Zip(harm, amp, ring), ClipNoise.ar(Seq(0.01, 0.01)), "freq".ir(300.0))
    Out.ar("out".kr(0.0), klank)
  }

  //////////////////////////////////////////////////////////////////////////

  // ambient by tim walters (added a LeakDC)
  dfs += "AmbientWalters" -> SynthGraph {
    val res = GVerb.ar(LeakDC.ar(
      Mix.tabulate(16) { k =>
        Mix.tabulate(6) { i =>
          val x = Impulse.kr(0.5.pow(i) / k)
          SinOsc.ar(i, SinOsc.ar((i + k).pow(i)) / (Decay.kr(x, Seq(i, i + 1)) * k))
        }
      }
    ), roomSize = 1) / 384
    WrapOut(res)
  }

  //////////////////////////////////////////////////////////////////////////

  // SysSon problem patch
  dfs += "SysSon-Test" -> SynthGraph {
    val v = LFDNoise3.ar(0.1).linExp(-1, 1, 1e-5, 1)
    val n = 32
    var mix: GE = 0
    for (_ <- 0 until n) {
      val freq = LFDNoise3.ar(LFDNoise3.ar(v).linExp(-1, 1, 1e-4, 0.01)).linExp(-1, 1, 64, 16000)
      val sin = SinOsc.ar(freq)
      val mul = LFDNoise3.ar(LFDNoise3.ar(v).linExp(-1, 1, 1e-4, 1)).linExp(-1, 1, 0.001, 1)
      mix += sin * mul
    }
    val sig = OnePole.ar(mix / n * 2, 0.95)
    Out.ar(0, Pan2.ar(sig))
  }

  ////////////////////////////////////////

  // Env
  dfs += "Env" -> SynthGraph {
    val segm = {
      import Curve._
      Seq(step, linear, exponential, sine, welch, parametric(-2f), squared, cubed).zipWithIndex.map { case (c, i) =>
        Env.Segment(0.1, i + 0.1, c)
      }
    }
    val env  = Env(0.01, segm)
    val y = EnvGen.ar(env)
    val z = EnvGen.ar(Env.perc, doneAction = freeSelf) * SinOsc.ar(440) * 0.1
    Out.ar(0, y + z)
  }

  dfs.foreach { case (name, graph) =>
    describe(s"SynthGraph '$name' ") {
      it("should serialize and de-serialize") {
        val out = DataOutput()
        SynthGraphObj.valueFormat.write(graph, out)
        val in  = DataInput(out.toByteArray)
        val res = SynthGraphObj.valueFormat.read(in)
        assert(res === graph)
      }
    }
  }
}