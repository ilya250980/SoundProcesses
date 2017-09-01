package de.sciss.synth.proc

import de.sciss.file._
import de.sciss.lucre.stm.store.BerkeleyDB

object MacroTest {
  import CompilerImplicits._

  def main(args: Array[String]): Unit = {
    type S      = Durable
    val dir     = File.createTempIn(userHome, directory = true)
    val ds      = BerkeleyDB.factory(dir)
    val ws      = Workspace.Durable.empty(dir, ds)
    ws.cursor.step { implicit tx =>
      val p = Proc[S]
      import de.sciss.synth._
      import ugen._
      p.setGraphWithSource {
        val o = LFSaw.kr(Seq(8, 7.23)).madd(3, 80)
        val f = LFSaw.kr(0.4).madd(24, o)
        val s = SinOsc.ar(f.midicps) * 0.04
        val c = CombN.ar(s, 0.2, 0.2, 4)
        val l = Line.kr(1, 0, 10, doneAction = freeSelf)
        Out.ar(0, c * l)
      }
      println(s"Graph: ${p.graph().value}")
      println("---- Source ----")
      println(p.attr.$[Code.Obj](Proc.attrSource).map(_.value.source).getOrElse("<not found>"))
      println("----------------")
    }
  }
}