package de.sciss
package synth
package proc

import de.sciss.lucre.synth.{Server, Txn}
import de.sciss.span.Span
import de.sciss.synth.message.{GroupDumpTree, Responder, Status, StatusReply}

import scala.concurrent.Promise
import scala.util.Success

/*
  To run only this suite:

  test-only de.sciss.synth.proc.Issue28

  */
class Issue28 extends BounceSpec {
  "Auxiliary attribute map synths are correctly freed" works { implicit cursor =>

    val tlH = cursor.step { implicit tx =>
      val _p1 = proc {
        import ugen._
        import graph._
        ScanOut(WhiteNoise.ar(0.1))     // actual signal has no significance
      }
      val _p2 = proc {
        import ugen._
        import graph._
        ScanOut(SinOsc.ar(441) * 0.1)   // actual signal has no significance
      }
      val _p3 = proc {
        import ugen._
        import graph._
        val sig = ScanInFix(numChannels = 1)
        Out.ar(0, sig)
      }

      addOutput(_p1) ~> (_p3 -> "in")
      addOutput(_p2) ~> (_p3 -> "in")

      val tl = Timeline[S]
      tl.add(Span(1.0.seconds, 3.0.seconds), _p1)
      tl.add(Span(2.0.seconds, 4.0.seconds), _p2)
      tl.add(Span.All                      , _p3)

      tx.newHandle(tl)
    }

    val res = Promise[StatusReply]()
    val as: AuralSystem = AuralSystem()

    def runTL(s: Server)(implicit tx: S#Tx): Unit = {
      // println("Here [1]")
      import WorkspaceHandle.Implicits.dummy
      val t   = Transport[S](as)
      val tl  = tlH()
      t.addObject(tl)
      t.play()
      t.scheduler.schedule(5.0.seconds) { implicit tx =>
        // println("Here [2]")
        t.dispose()
        tx.afterCommit {
          Responder.once(s.peer) {
            case m: StatusReply =>
              s.peer.quit()
              res.complete(Success(m))
          }
          s ! GroupDumpTree(1 -> false)
          s ! Status
        }
      }
    }

    cursor.step { implicit tx =>
      as.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit itx: Txn): Unit = {
          implicit val tx: S#Tx = cursor.wrap(itx.peer)
          runTL(s)
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      as.start()
    }

    res.future.map { status =>
      assert(status.numGroups === 2)
      assert(status.numSynths === 0)
    }
  }
}