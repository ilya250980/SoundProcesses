package de.sciss.proc

import de.sciss.lucre.synth.Server
import de.sciss.span.Span
import de.sciss.synth.message.{GroupDumpTree, Responder, Status, StatusReply}
import de.sciss.synth.proc.graph
import de.sciss.synth

import scala.concurrent.Promise
import scala.util.Success

/*
  To run only this suite:

  testOnly de.sciss.proc.Issue28

  */
class Issue28 extends BounceSpec {
  "Auxiliary attribute map synths are correctly freed" works { implicit universe =>
    import universe.cursor

    val tlH = cursor.step { implicit tx =>
      import Implicits._
      val _p1 = mkProc {
        import synth._
        import graph._
        import ugen._
        ScanOut(WhiteNoise.ar(0.1))     // actual signal has no significance
        ()
      }
      _p1.name = "noise"
      val _p2 = mkProc {
        import synth._
        import graph._
        import ugen._
        ScanOut(SinOsc.ar(441) * 0.1)   // actual signal has no significance
        ()
      }
      _p2.name = "sine"
      val _p3 = mkProc {
        import synth._
        import graph._
        import ugen._
        val sig = ScanInFix(numChannels = 1)
        Out.ar(0, sig)
        ()
      }
      _p3.name = "main"

      addOutput(_p1) ~> (_p3 -> "in")
      addOutput(_p2) ~> (_p3 -> "in")

      val tl = Timeline[T]()
      tl.add(Span(1.0.seconds, 3.0.seconds), _p1)
      tl.add(Span(2.0.seconds, 4.0.seconds), _p2)
      tl.add(Span.All                      , _p3)

      tx.newHandle(tl)
    }

    val pStatus = Promise[StatusReply]()
//    implicit val universe: Universe[T] = cursor.step { implicit tx => Universe.dummy }

    def runTL(s: Server)(implicit tx: T): Unit = {
//      println("Here [1]")
      val t   = Transport[T](universe)
      val tl  = tlH()
      t.addObject(tl)
      t.play()
      universe.scheduler.schedule(6.0.seconds) { implicit tx =>
//        println("Here [2]")
        // t.dispose()
        tx.afterCommit {
          Responder.once(s.peer) {
            case m: StatusReply =>
              s.peer.quit()
              // there is a weird thing, where peer.quit
              // may cause a second status reply to come in,
              // despite `Responder.once`, so make it a "try"
              pStatus.tryComplete(Success(m))
              ()
          }
          s ! GroupDumpTree(1 -> false)
          s ! Status
        }
      }
      ()
    }

    val res = runServer() { s =>
      cursor.step { implicit tx =>
        runTL(s)
        pStatus.future
      }
    }

    res.map { status =>
      assert(status.numGroups === 2)
      assert(status.numSynths === 1)    // only global should be left
    }
  }
}