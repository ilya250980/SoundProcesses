package de.sciss.synth.proc.tests

import de.sciss.lucre.expr.{Context, Graph, graph}
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{ExprContext, Universe}

object Issue70 extends App {
  type S = InMemory
  type T = InMemory.Txn

  val g = Graph {
    import graph._

    LoadBang() ---> Delay(1.0)(PrintLn("Henlo"))
  }

  import de.sciss.lucre.Workspace.Implicits._

  implicit val system : S = InMemory()

  system.step { implicit tx =>
    implicit val undo   : UndoManager [T] = UndoManager()
    implicit val u      : Universe    [T] = Universe()
    implicit val ctx    : Context     [T] = ExprContext[T]()

    val c = g.expand
    c.initControl()
    c.dispose()
  }
}

