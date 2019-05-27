package de.sciss.synth.proc

import de.sciss.file.File
import de.sciss.lucre.stm.Folder
import de.sciss.span.Span
import de.sciss.synth.proc.graph.{ScanInFix, ScanOut}
import de.sciss.synth.ugen

import scala.concurrent.ExecutionContext

/*

testOnly de.sciss.synth.proc.Issue53

 */
class Issue53 extends BounceSpec {
  "Aural outputs" works { implicit universe =>
    import universe.cursor

    val freq1 = sampleRate/2
    val freq2 = 441.0
    val freq3 = 220.5

    val tim0  = 1.0
    val tim1  = 2.0
    val atH   = frame(tim0)
    val atF   = frame(tim1)
    val atHf  = tim0.secondsFileI
    val atFf  = tim1.secondsFileI

    import ugen._
    val tlH = cursor.step { implicit tx =>
      val proc1 = proc {
        ScanOut(LFPulse.ar(freq1) * 0.5)
      }

      val proc2 = proc {
        ScanOut(SinOsc.ar(freq2) * 0.5)
      }

      val proc3 = proc {
        ScanOut(SinOsc.ar(freq3) * 0.5)
      }

      val glob = proc {
        val in = ScanInFix(1)
        Out.ar(0, in)
      }

      val f = Folder[S]()
      f.addLast(proc1.outputs.add(Proc.mainOut))
      f.addLast(proc2.outputs.add(Proc.mainOut))
      f.addLast(proc3.outputs.add(Proc.mainOut))
      glob.attr.put(Proc.mainIn, f)

      val tl = Timeline[S]()
      tl.add(Span.All, glob)
      tl.add(Span(0L, atH), proc1)
      tl.add(Span(0L, atH), proc2)
      tl.add(Span(atH, atF), proc3)

      tx.newHandle(tl)
    }

    val c = config(List(tlH), Span(0L, atF))
    c.server.nrtOutputPath = File.createTemp(suffix = ".aif", deleteOnExit = false).getPath
    c.realtime = true

    import ExecutionContext.Implicits.global
    // showTransportLog = true
    val r = bounce(c, debugKeep = true)
    r.map { case Array(observed) =>
      val sig1 = mkLFPulse(freq1, startFrame = 1, len = atHf, amp = 0.5)
      val sig2 = mkSine   (freq2, startFrame = 1, len = atHf, amp = 0.5)
      val sig3 = mkSine   (freq3, startFrame = 1, len = atHf, amp = 0.5)

      val expected = new Array[Float](atFf)
      add(expected, sig1)
      add(expected, sig2)
      add(expected, sig3, aOff = atHf)

      assertSameSignal(observed, expected)
      //      assert(true)
    } (global)
  }
}
