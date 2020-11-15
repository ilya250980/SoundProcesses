package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExImport._
import de.sciss.lucre.IntObj
import de.sciss.lucre.{Obj => LObj, Folder => LFolder}
import de.sciss.lucre.expr.graph._
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.synth.InMemory
import de.sciss.proc.{ExprContext, Universe}

object ExMapTest2 extends App {
  type S = InMemory
  type T = InMemory.Txn

  val g = Graph {
    val fAttr = "foo".attr[Folder]
    val m = fAttr.map(_.size)
    // LoadBang() ---> DebugAct(() => println("Henlo"))

    m.changed ---> PrintLn(m.toStr)
  }

  implicit val system: S = InMemory()

  system.step { implicit tx =>
    val self  = IntObj.newConst(0): IntObj[T]
    val f     = LFolder[T]()
    self.attr.put("foo", f)
    val selfH = tx.newHandle(self)
    implicit val u    : Universe    [T] = Universe.dummy
    implicit val undo : UndoManager [T] = UndoManager()
    implicit val ctx  : Context     [T] = ExprContext(Some(selfH))
    g.expand.initControl()
    val x: LObj[T] = IntObj.newConst(1)
    f.addLast(x)
  }
}
