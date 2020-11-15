package de.sciss.synth.proc

import de.sciss.lucre.Folder
import de.sciss.span.Span
import de.sciss.synth.proc.graph.{ScanInFix, ScanOut}
import de.sciss.synth.ugen

import scala.concurrent.ExecutionContext

/*

testOnly de.sciss.synth.proc.Issue53

 */
class Issue53 extends BounceSpec {
//  override def blockSize: Int = 64

  "Aural outputs" works { implicit universe =>
    import universe.cursor

    val freq1 = sampleRate/2
    val freq2 = 441.0
//    val freq3 = 220.5

    val tim0  = 0.5
    val tim1  = 1.5
    val tim2  = 2.5

//    val lenTol  = atFf.nextPowerOfTwo - atFf
//    println(lenTol)

//    val atFf  = 81920 + 64
//    val tim2  = atFf / sampleRate

    val at0   = frame(tim0)
    val at1   = frame(tim1)
    val at2   = frame(tim2)
    val at0f  = tim0.secondsFileI
    val at1f  = tim1.secondsFileI
    val at2f  = tim2.secondsFileI

    import ugen._
    val tlH = cursor.step { implicit tx =>
      val proc1 = proc {
        ScanOut(LFPulse.ar(freq1) * 0.5)
      }

      val proc2 = proc {
        ScanOut(SinOsc.ar(freq2) * 0.5)
      }

      // shitty real-time SC is never sample accurately scheduled,
      // so to compare signals we'll end making a DC instead
      val proc3 = proc {
//        ScanOut(SinOsc.ar(freq2) * 0.5)
        ScanOut(DC.ar(0.33))
      }

      val glob = proc {
        val in = ScanInFix(1)
        Out.ar(0, in)
      }

      val f = Folder[T]()
      f.addLast(proc1.outputs.add(Proc.mainOut))
      f.addLast(proc2.outputs.add(Proc.mainOut))
      f.addLast(proc3.outputs.add(Proc.mainOut))
      glob.attr.put(Proc.mainIn, f)

      val tl = Timeline[T]()
      tl.add(Span.All, glob)
      tl.add(Span(at0, at1), proc1)
      tl.add(Span(at0, at1), proc2)
      tl.add(Span(at1, at2), proc3)

      tx.newHandle(tl)
    }

    val c = config(List(tlH), Span(0L, at2))
//    c.server.nrtOutputPath    = File.createTemp(suffix = ".aif", deleteOnExit = false).getPath
//    c.server.nrtHeaderFormat  = AudioFileType.IRCAM
    c.client.latency  = 0.2
    c.realtime = true

//    BounceImpl.DEBUG  = true
//    ServerImpl.DEBUG  = true
//    ServerImpl.USE_COMPRESSION = true
//    c.beforePlay = { case (_tx, s) =>
//      implicit val tx: S#Tx = _tx
//      s.peer.dumpOSC()
//    }

//    showTransportLog  = true
//    showAuralLog      = true

    import ExecutionContext.Implicits.global
    // showTransportLog = true
    val r = bounce(c)
    r.map { case Array(observed0) =>

      val sig1 = mkLFPulse(freq1, startFrame = 1, len = at1f - at0f, amp = 0.5)
      val sig2 = mkSine   (freq2, startFrame = 1, len = at1f - at0f, amp = 0.5)
//      val sig3 = mkSine   (freq3, startFrame = 1, len = atHf, amp = 0.5)
//      val sig3 = mkSine   (freq2, startFrame = 1, len = at2f - at1f, amp = 0.5)
      val sig3 = mkDC(len = at2f - at1f, amp = 0.33)

      val expected = new Array[Double](at2f - at0f)
      add(expected, sig1, aOff = 0 /*at0f*/)
      add(expected, sig2, aOff = 0 /*at0f*/)
      add(expected, sig3, aOff = at1f - at0f)

//      {
//        val af = AudioFile.openWrite(userHome / "bla.aif", AudioFileSpec(sampleRate = sampleRate, numChannels = 1))
//        af.write(Array(expected))
//        af.close()
//      }

      val observed = observed0.dropWhile(_ == 0f)

      // XXX TODO --- why is the file so much shorter?
      // Might be a problem in SC 3.9 that got fixed in 3.10; need to investigate
      assertSameSignal(observed, expected, tol = 0.05f, lengthTol = 32768 /* ! */, dropOuts = 64)
      //      assert(true)
    } (global)
  }
}
