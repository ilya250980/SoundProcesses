package de.sciss.synth.proc

import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.synth
import de.sciss.synth.SynthGraph

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.concurrent.stm.Txn

object ActionGraphTest extends App {
  val confluent = true   // currently test4 has a problem with event-variables in confluent

  val name = args.headOption.getOrElse("?")

  if (confluent) {
    type S  = Confluent
//    type I  = S#I
    val sys = Confluent(BerkeleyDB.tmp())
    val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
    new ActionGraphTest[S]()(cursor)

  } else {
    type S  = Durable
//    type I  = S#I
    val sys = Durable(BerkeleyDB.tmp())
    val cursor: stm.Cursor[S] = sys
    new ActionGraphTest[S]()(cursor)
  }
}
class ActionGraphTest[S <: Sys[S]]()(implicit cursor: stm.Cursor[S]) {
  val as = AuralSystem()
  cursor.step { implicit tx =>
    as.whenStarted(s => initView(as, s))
    as.start()
  }
  
  def initView(as: AuralSystem, s: Server): Unit = {
    if (Txn.findCurrent.isDefined) {
      Console.err.println("Damn! I could swear there is no transaction.")
      throw new IllegalStateException()
    }

    s.peer.dumpOSC()
//    implicit val context = cursor.step { implicit tx =>
//      import WorkspaceHandle.Implicits._
//      AuralContext[S](s)
//    }

    print("Compiling...")
    implicit val compiler: Code.Compiler = Compiler()

    val actionFut = cursor.step { implicit tx =>
      val code = Code.ActionRaw(
        """val name = self.attr[StringElem]("name").map(_.value).getOrElse("<not-found>")
          |println(s"Bang! My name is $name")
          |sys.exit(0)
        """.stripMargin)
      ActionRaw.compile[S](code)
    }

    val actionH = Await.result(actionFut, Duration.Inf)
    println(" ok.")

    cursor.step { implicit tx =>
      val p = Proc[S]
      // import ExprImplicits._

      p.graph() = SynthGraph {
        import synth._
        import ugen._
        val tr    = Impulse.kr(2)
        val count = PulseCount.kr(tr)
        count.poll(tr, "count")
        val bang  = count > 9.5
        graph.Action(bang, "foo")
      }

      val obj = p // Obj(Proc.Elem(p))
      val actionObj = actionH() // Obj(Action.Elem(actionH()))
      actionObj.attr.put("name", "Baba Ganoush": StringObj[S])
      obj.attr.put("foo", actionObj)

      implicit val universe: Universe[S] = ??? // UUU
      val t = Transport[S](universe)
      t.addObject(obj)
      t.play()
    }

    println("Now playing...")
  }
}