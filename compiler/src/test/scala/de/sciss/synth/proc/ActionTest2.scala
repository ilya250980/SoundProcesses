package de.sciss.synth.proc

import de.sciss.file.File
import de.sciss.lucre.stm.store.BerkeleyDB

import scala.concurrent.ExecutionContext
import scala.util.Failure

object ActionTest2 extends App {
  type S = Confluent
  val dir     = File.createTemp(directory = true)
  val factory = BerkeleyDB.factory(dir)
  val system  = Confluent(factory)
  val (_, cursor0) = system.cursorRoot(_ => ()) { implicit tx => _ => system.newCursor() }
  implicit val cursor = cursor0

  val source1 =
    """val xs = List("hello", "world")
      |println(xs.map(_.capitalize).mkString(" "))
      |""".stripMargin

  implicit val compiler = Compiler()
  import ExecutionContext.Implicits.global

  val futAction = cursor.step { implicit tx =>
    val code    = Code.Action(source1)
    println("Starting compilation...")
    Action.compile(code)
  }

  futAction.onComplete {
    case Failure(e) =>
      println("Compilation failed!")
      e.printStackTrace()
    case _ =>
  }

  futAction.foreach { actionH =>
    println("Compilation completed.")
    cursor.step { implicit tx =>
      val action = actionH()
      println("Execute #1")
      action.execute(null)
    }
    cursor.step { implicit tx =>
      val action = actionH()
      println("Execute #2")
      action.execute(null)
    }
    sys.exit()
  }
}
