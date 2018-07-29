package de.sciss.synth.proc

import de.sciss.lucre.expr
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.swing.{Graph => WGraph, graph => wgraph}
import org.scalatest.{Matchers, Outcome, fixture}

class SerializationSpec extends fixture.FlatSpec with Matchers {
  type S = Durable
  type FixtureParam = S

  SoundProcesses.init()
  Widget        .init()

  protected def withFixture(test: OneArgTest): Outcome = {
    val store  = BerkeleyDB.tmp()
    val system = Durable(store)
    try {
      test(system)
    } finally {
      system.close()
    }
  }

  "An Widget object" should "be serializable" in { cursor =>
    val (wH, gIn) = cursor.step { implicit tx =>
      val w = Widget[S]
      val g = WGraph {
        import expr.ExOps._
        import wgraph._
        val sl      = Slider()
        sl.min      = 1
        sl.max      = 10
        sl.value()  = "foo".attr(1)
        val txt = (sl.value().dbAmp * 2.0).toStr
        val lb = Label(txt)
        FlowPanel(sl, lb)
      }
      w.graph() = g
      tx.newHandle(w) -> g
    }

    cursor.step { implicit tx =>
      val w = wH()
      val gOut = w.graph.value
      assert(gIn === gOut)
    }
  }
}