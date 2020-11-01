package de.sciss.synth.proc

import de.sciss.span.Span
import de.sciss.synth.ugen

import scala.concurrent.ExecutionContext

/*

testOnly de.sciss.synth.proc.AuralSpecs

 */
class AuralSpecs extends BounceSpec {
  "A proc with a sine oscillator" works { implicit universe =>
    import universe.cursor
    val freq = 441

    val pH = cursor.step { implicit tx =>
      val _p = proc {
        import ugen._
        Out.ar(0, SinOsc.ar(freq))
        ()
      }
      tx.newHandle(_p)
    }

    val c = config(pH, Span(0, 1.0.seconds))
    import ExecutionContext.Implicits.global
    val r = bounce(c)

//    r.onComplete {
//      res => println(s"RES $res")
//    } (global)

    r.map { case Array(arr) =>
      val man = mkSine(freq, startFrame = 1, len = 1.0.secondsFileI)
      assertSameSignal(arr, man)
    } (global)
  }

  "Two connected procs (generator -> filter)" works { implicit universe =>
    import universe.cursor

    val freq  = 441
    val amp   = 0.5

    import graph.Ops.stringToControl
    import ugen._
    val (pH1, pH2) = cursor.step { implicit tx =>
      val proc1 = proc {
        val amp   = "amp".ir(0.0)
        val noise = LFPulse.ar(SampleRate.ir * 0.5) * Seq(amp, amp)
        graph.ScanOut("out", noise)
        ()
      }
      import Implicits._
      proc1.name = "sine"
      doubleAttr(proc1, "amp", amp)

      val proc2 = proc {
        val freq  = "freq".ir(111.0)
        val in    = graph.ScanIn("in")
        Out.ar(0, SinOsc.ar(freq) * in)
        ()
      }
      proc2.name = "mod"
      doubleAttr(proc2, "freq", freq)

      (tx.newHandle(proc1), tx.newHandle(proc2))
    }

    val c = config(List(pH1, pH2), Span(0, 1.0.seconds), numChannels = 2)
    action(c, 0.5.seconds) { implicit tx =>
      val proc1   = pH1()
      val proc2   = pH2()
      val scanOut = addOutput(proc1, "out")
      val scanIn  = (proc2, "in" )
      scanOut ~> scanIn
    }

    import ExecutionContext.Implicits.global
    // showTransportLog = true
    val r = bounce(c)
    r.map { case Array(arr0, arr1) =>
      val flt   = mkLFPulse(sampleRate / 2, startFrame = 1 /* 0.5.secondsFileI */, len = 0.5.secondsFileI)
      mulScalar(flt, 0.5f)
      val mod   = mkSine(freq, startFrame = 1 /* 0.5.secondsFileI */, len = 0.5.secondsFileI)
      mulArray(flt, mod)
      val man   = mkSilent(0.5.secondsFileI) ++ flt
//      import de.sciss.synth.swing.Plotting.Implicits._
//      scala.swing.Swing.Swing.onEDTWait {
////        flt.toVector.plot("flt")
////        mod.toVector.plot("mod")
//        man .toVector.plot(title = "man" )
//        arr0.toVector.plot(title = "arr0")
//      }
//      Thread.sleep(100000)
      assertSameSignal(arr0, man)
      assertSameSignal(arr1, man)
//      assert(true)
    } (global)
  }
}
