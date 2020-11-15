package de.sciss.synth.proc.tests

import de.sciss.log.Level
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.{IntObj, Workspace}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.SoundProcesses.logAural
import de.sciss.synth.proc.{Grapheme, Proc, SoundProcesses, TimeRef, Transport, Universe}

object Issue35 {
  type S = InMemory
  type T = InMemory.Txn
  implicit val cursor: S = InMemory()

  logAural    .level  = Level.Debug

//  type S = Durable
//  implicit val cursor: stm.Cursor[T] = {
//    val ds = BerkeleyDB.tmp()
//    Durable(ds)
//  }

  implicit val ws: Workspace[T] = Workspace.Implicits.dummy

  SoundProcesses.init()
  val universe: Universe[T] = cursor.step { implicit tx => Universe.dummy }

  cursor.step { implicit tx =>
    universe.auralSystem.whenStarted { _ =>
      cursor.step { implicit tx =>
        val t = Transport[T](universe)
        run(t)
      }
    }
    universe.auralSystem.start()
  }

  def main(args: Array[String]): Unit = ()

  def run(t: Transport[T])(implicit tx: T): Unit = {
    val gr = SynthGraph {
      import de.sciss.synth._
      import de.sciss.synth.proc.graph.Ops._
      import de.sciss.synth.ugen._

      val pitch     = "pitch".ar
      val strike    = (pitch sig_!= Delay2.ar(pitch)) * 0.05
      pitch.poll(     0, "not0")
      pitch.poll(strike, "note")
    }

    val p = Proc[T]()
    p.graph() = gr

    val g = Grapheme[T]()
    (78 to 86).zipWithIndex.foreach { case (pch, beat) =>
      val time = (beat * 0.5 * TimeRef.SampleRate).toLong
      g.add(time, IntObj.newConst(pch))
    }
    p.attr.put("pitch", g)

    t.addObject(p)
    t.play()
  }
}