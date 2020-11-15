package de.sciss.lucre.expr

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.synth.InMemory
import de.sciss.proc.{ExprContext, Universe}

object DelayTest extends App {
  type S = InMemory
  type T = InMemory.Txn

  val g = Graph {
    import graph._

    val b = LoadBang()
    b                 ---> PrintLn("Henlo")
    b ---> Delay(2.0) ---> PrintLn("World")
  }

  implicit val system: S = InMemory()

  system.step { implicit tx =>
    implicit val u    : Universe    [T] = Universe.dummy
    implicit val undo : UndoManager [T] = UndoManager()
    implicit val ctx  : Context     [T] = ExprContext()
    g.expand[T].initControl()
  }
}
