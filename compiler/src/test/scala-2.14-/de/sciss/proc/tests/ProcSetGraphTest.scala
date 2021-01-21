package de.sciss.proc.tests

import de.sciss.file._
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.proc.{Code, Durable, MacroImplicits, Proc, SoundProcesses, Workspace}

object ProcSetGraphTest {
  import MacroImplicits._

  /** Creates a demo workspace in `~/Documents` that we should be able to verify from Mellite. */
  def main(args: Array[String]): Unit = {
    SoundProcesses.init()

    type T      = Durable.Txn
    val dir     = File.createTempIn(userHome / "Documents", suffix = ".mllt", directory = true)
    val ds      = BerkeleyDB.factory(dir)
    val ws      = Workspace.Durable.empty(dir.toURI, ds)
    ws.cursor.step { implicit tx =>
      val p = Proc[T]()
      import de.sciss.synth._
      import ugen._
      p.setGraph {
        // bubbles here
        val o = LFSaw.kr(Seq(8, 7.23)).mulAdd(3, 80)
        val f = LFSaw.kr(0.4).mulAdd(24, o)
        val s = SinOsc.ar(f.midiCps) * 0.04
        val c = CombN.ar(s, 0.2, 0.2, 4)
        val l = Line.kr(1, 0, 10, doneAction = freeSelf)
        Out.ar(0, c * l)
        ()
      }
      ws.root.addLast(p)

      println(s"Graph: ${p.graph().value}")
      println("---- Source ----")
      println(p.attr.$[Code.Obj](Proc.attrSource).map(_.value.source).getOrElse("<not found>"))
      println("----------------")
    }

    ws.cursor.step { implicit tx => ws.dispose() }

    sys.exit()
  }
}