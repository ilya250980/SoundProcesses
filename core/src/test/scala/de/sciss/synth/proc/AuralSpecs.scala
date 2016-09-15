package de.sciss.synth.proc

import de.sciss.span.Span
import de.sciss.synth.ugen

class AuralSpecs extends BounceSpec {
  "A proc with a sine oscillator" should "produce the predicted sound output" in { implicit cursor =>
    val freq = 441

    val pH = cursor.step { implicit tx =>
      val _p = proc {
        import ugen._
        Out.ar(0, SinOsc.ar(freq))
      }
      tx.newHandle(_p)
    }

    val c = config(pH, Span(0, 1.0.seconds))
    val r = bounce(c)
    r.map { case Array(arr) =>
      val man = mkSine(freq, startFrame = 1, len = sampleRateI)
      assertSameSignal(arr, man)
    }
  }
}
