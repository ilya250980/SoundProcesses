package de.sciss.synth.proc.gui

import de.sciss.lucre.expr.Graph
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc
import de.sciss.synth.proc.{Control, Universe}

// expected output: `Some(440.0)`
object RunWithTest extends App {
  type S = InMemory
  type T = InMemory.Txn

  val gSource = Graph {
//    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    val r = Runner("that")
    // Ex[Seq[...]] should be both inferred for
    // Seq("foo" -> 123.0) and Seq("foo" -> (123.0: Ex[Double]))
    val freq: Ex[Double] = 440.0
    val a = r.runWith("freq" -> freq)

    LoadBang() ---> a
  }

  val gSink = Graph {
    import de.sciss.lucre.expr.ExImport._
    import de.sciss.lucre.expr.graph._
    val fAttr: Ex[Option[Double]] = "freq".attr[Double]
    LoadBang() ---> PrintLn(fAttr.toStr)
  }

  implicit val system: S = InMemory()

  system.step { implicit tx =>
    val c1 = Control[T]()
    c1.graph() = Control.GraphObj.newConst[T](gSource)
    val c2 = Control[T]()
    c2.graph() = Control.GraphObj.newConst[T](gSink)
    c1.attr.put("that", c2)
    implicit val u: Universe[T] = Universe.dummy
    val r1 = proc.Runner(c1)
    r1.run()
  }
}