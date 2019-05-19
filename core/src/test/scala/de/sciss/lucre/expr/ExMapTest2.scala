package de.sciss.lucre.expr

import de.sciss.lucre.expr.ExImport._
import de.sciss.lucre.expr.graph._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{InMemory, Workspace}

object ExMapTest2 extends App {
  type S = InMemory

  val g = Graph {
    val fAttr = "foo".attr[Folder]
    val m = fAttr.map(_.size)
    // LoadBang() ---> DebugAct(() => println("Henlo"))

    m.changed ---> PrintLn(m.toStr)
  }

  implicit val system: S = InMemory()

  import Workspace.Implicits._

  system.step { implicit tx =>
    val self  = IntObj.newConst(0): IntObj[S]
    val f     = stm.Folder[S]()
    self.attr.put("foo", f)
    val selfH = tx.newHandle(self)
    implicit val ctx: Context[S] = Context(Some(selfH))
    g.expand.initControl()
    val x: stm.Obj[S] = IntObj.newConst(1)
    f.addLast(x)
  }
}
