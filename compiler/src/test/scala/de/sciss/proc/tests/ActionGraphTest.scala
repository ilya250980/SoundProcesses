//package de.sciss.proc.tests
//
//import de.sciss.lucre.{Cursor, StringObj, Txn}
//import de.sciss.lucre.store.BerkeleyDB
//import de.sciss.lucre.synth.{Server, Sys}
//import de.sciss.synth
//import de.sciss.synth.SynthGraph
//import de.sciss.proc.{AuralSystem, Code, Compiler, Confluent, Durable, Proc, Transport, Universe, graph}
//
//import scala.concurrent.Await
//import scala.concurrent.duration.Duration
//import scala.concurrent.stm.{Txn => STMTxn}
//
//object ActionGraphTest extends App {
//  val confluent = true   // currently test4 has a problem with event-variables in confluent
//
//  val name = args.headOption.getOrElse("?")
//
//  if (confluent) {
//    type S  = Confluent
//    type T  = Confluent.Txn
////    type I  = S#I
//    val sys = Confluent(BerkeleyDB.tmp())
//    val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
//    new ActionGraphTest[T]()(cursor)
//
//  } else {
//    type S  = Durable
//    type T  = Durable.Txn
////    type I  = S#I
//    val sys = Durable(BerkeleyDB.tmp())
//    val cursor: Cursor[T] = sys
//    new ActionGraphTest[T]()(cursor)
//  }
//}
//class ActionGraphTest[T <: Txn[T]]()(implicit cursor: Cursor[T]) {
//  val as = AuralSystem()
//  cursor.step { implicit tx =>
//    as.whenStarted(s => initView(as, s))
//    as.start()
//  }
//
//  def initView(as: AuralSystem, s: Server): Unit = {
//    if (STMTxn.findCurrent.isDefined) {
//      Console.err.println("Damn! I could swear there is no transaction.")
//      throw new IllegalStateException()
//    }
//
//    s.peer.dumpOSC()
////    implicit val context = cursor.step { implicit tx =>
////      import WorkspaceHandle.Implicits._
////      AuralContext[T](s)
////    }
//
//    print("Compiling...")
//    implicit val compiler: Code.Compiler = Compiler()
//
//    val actionFut = cursor.step { implicit tx =>
//      val code = Code.ActionRaw(
//        """val name = self.attr[StringElem]("name").map(_.value).getOrElse("<not-found>")
//          |println(s"Bang! My name is $name")
//          |sys.exit(0)
//        """.stripMargin)
//      ActionRaw.compile[T](code)
//    }
//
//    val actionH = Await.result(actionFut, Duration.Inf)
//    println(" ok.")
//
//    cursor.step { implicit tx =>
//      val p = Proc[T]()
//      // import ExprImplicits._
//
//      p.graph() = SynthGraph {
//        import synth._
//        import ugen._
//        val tr    = Impulse.kr(2)
//        val count = PulseCount.kr(tr)
//        count.poll(tr, "count")
//        val bang  = count > 9.5
//        graph.Action(bang, "foo")
//      }
//
//      val obj = p // Obj(Proc.Elem(p))
//      val actionObj = actionH() // Obj(Action.Elem(actionH()))
//      actionObj.attr.put("name", "Baba Ganoush": StringObj[T])
//      obj.attr.put("foo", actionObj)
//
//      implicit val universe: Universe[T] = ??? // UUU
//      val t = Transport[T](universe)
//      t.addObject(obj)
//      t.play()
//    }
//
//    println("Now playing...")
//  }
//}