package de.sciss.synth.proc.gui

import de.sciss.lucre.expr.Graph
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.Widget

object RunWithTest extends App {
  type S = InMemory

  val gSource = Graph {
//    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    val r = Runner("that")
    // Ex[Seq[...]] should be both inferred for
    // Seq("foo" -> 123.0) and Seq("foo" -> (123.0: Ex[Double]))
    val freq: Ex[Double] = 440.0
    val a = r.runWith(
      Seq(
        "freq" -> freq
      )
    )

    LoadBang() ---> a
//    Empty()
  }

  val gSink = SynthGraph {
    import de.sciss.synth.proc.graph.Ops.stringToControl
//    import de.sciss.synth.proc.graph._
    val fAttr = "freq".ar(0.0)
    fAttr.poll(0, "freq")
  }

  implicit val system: S = InMemory()

  system.step { implicit tx =>
    val w1 = Widget[S]()
//    w1.graph() = Widget.GraphObj.newConst[S](gSource)

    ???
//    val self  = IntObj.newConst(0): IntObj[S]
//    val f     = stm.Folder[S]()
//    self.attr.put("foo", f)
//    val selfH = tx.newHandle(self)
//    implicit val u    : Universe    [S] = Universe.dummy
//    implicit val undo : UndoManager [S] = UndoManager()
//    implicit val ctx  : Context     [S] = ExprContext(Some(selfH))
//    gSource.expand.initControl()
//    val x: stm.Obj[S] = IntObj.newConst(1)
//    f.addLast(x)
  }
}