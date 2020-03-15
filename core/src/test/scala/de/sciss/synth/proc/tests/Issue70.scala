package de.sciss.synth.proc.tests

import de.sciss.lucre.expr.{Context, Graph, graph}
import de.sciss.lucre.stm.{UndoManager, Workspace}
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{ExprContext, Universe}

object Issue70 extends App {
  type S = InMemory

  val g = Graph {
    import graph._

    LoadBang() ---> Delay(1.0)(PrintLn("Henlo"))
  }

  import Workspace.Implicits._

  implicit val system : S = InMemory()

  system.step { implicit tx =>
    implicit val undo   : UndoManager [S] = UndoManager()
    implicit val u      : Universe    [S] = Universe()
    implicit val ctx    : Context     [S] = ExprContext[S]()

    val c = g.expand
    c.initControl()
    c.dispose()
  }
}

