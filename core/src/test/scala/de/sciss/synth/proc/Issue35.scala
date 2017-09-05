package de.sciss.synth.proc

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.SynthGraph

object Issue35 {
  type S = InMemory
  implicit val cursor: stm.Cursor[S] = InMemory()
  implicit val ws: WorkspaceHandle[S] = WorkspaceHandle.Implicits.dummy
  val aural = AuralSystem()

  cursor.step { implicit tx =>
    aural.whenStarted { _ =>
      cursor.step { implicit tx =>
        val t = Transport[S](aural)
        run(t)
      }
    }
    aural.start()
  }

  def main(args: Array[String]): Unit = ()

  def run(t: Transport[S])(implicit tx: S#Tx): Unit = {
    val gr = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.proc.graph.Ops._
      import de.sciss.synth.ugen._

      val pitch     = "pitch".ar
      val strike    = (pitch sig_!= Delay2.ar(pitch)) * 0.05
      pitch.poll(     0, "not0")
      pitch.poll(strike, "note")
    }

    val p = Proc[S]
    p.graph() = gr

    val g = Grapheme[S]
    (78 to 86).zipWithIndex.foreach { case (pch, beat) =>
      val time = (beat * 0.5 * TimeRef.SampleRate).toLong
      g.add(time, IntObj.newConst(pch))
    }
    p.attr.put("pitch", g)

    t.addObject(p)
    t.play()
  }
}