package de.sciss.synth.proc.tests

import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{Grapheme, Proc, SoundProcesses, TimeRef, Transport, Universe, Workspace, showAuralLog}

object Issue35 {
  type S = InMemory
  implicit val cursor: S = InMemory()

  showAuralLog = true

//  type S = Durable
//  implicit val cursor: stm.Cursor[S] = {
//    val ds = BerkeleyDB.tmp()
//    Durable(ds)
//  }

  implicit val ws: Workspace[S] = Workspace.Implicits.dummy

  SoundProcesses.init()
  val universe: Universe[S] = cursor.step { implicit tx => Universe.dummy }

  cursor.step { implicit tx =>
    universe.auralSystem.whenStarted { _ =>
      cursor.step { implicit tx =>
        val t = Transport[S](universe)
        run(t)
      }
    }
    universe.auralSystem.start()
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