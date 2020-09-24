package de.sciss.lucre.expr

import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.{ExprContext, Universe}

object OscTest extends App {
  type S = InMemory
  type T = InMemory.Txn

  val g = Graph {
    import graph._
    val n = OscUdpNode(40000)
    val m = OscUdpNode(40001)
    m.dump = 1

//    val value: Ex[Int] = 123

    val msg = OscMessage.apply("/test", 123)

    val act = n.send(m.self, msg)
    LoadBang() ---> act

    m.received ---> PrintLn(m.message.name)

    val ch    = Var[Int]()
    val relay = Var[Boolean]()
    val sel   = n.message.select("/iterate", ch, relay)

    val text  = Const("Match!") ++ ch.toStr

    sel ---> PrintLn(text)
  }

  implicit val system: S = InMemory()

  system.step { implicit tx =>
    implicit val u    : Universe    [T] = Universe.dummy
    implicit val undo : UndoManager [T] = UndoManager()
    implicit val ctx  : Context     [T] = ExprContext()
    g.expand[T].initControl()
  }
}
