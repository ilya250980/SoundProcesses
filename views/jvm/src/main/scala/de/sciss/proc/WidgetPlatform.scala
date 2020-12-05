package de.sciss.proc

import de.sciss.lucre.swing.graph.{Widget => _Widget}
import de.sciss.lucre.swing.{Graph => _Graph}
import de.sciss.synth.UGenSource.Vec
import de.sciss.proc
import de.sciss.proc.Code.{Example, Import}
import de.sciss.proc.Widget.Graph
import de.sciss.proc.impl.CodeImpl

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.Future

trait WidgetPlatform {
  // ---- Code ----

  object Code extends proc.Code.Type {
    final val id        = 6
    final val prefix    = "Widget"
    final val humanName = "Widget Graph"
    type Repr           = Code

    override def examples: ISeq[Example] = List(
      Example("Hello World", 'h',
        """val b = Bang()
          |b ---> PrintLn("Hello World!")
          |b
          |""".stripMargin
      )
    )

    override def defaultSource: String = s"${super.defaultSource}Empty()\n"

    def docBaseSymbol: String = "de.sciss.lucre.swing.graph"

    private[this] lazy val _init: Unit = {
      proc.Code.addType(this)
      import Import._
      proc.Code.registerImports(id, Vec(
        Import("de.sciss.numbers.Implicits", All),
//        Import("de.sciss.lucre.expr.ExImport", All),
        Import("de.sciss.proc.ExImport", All),
//        Import("de.sciss.file", All),
        Import("de.sciss.lucre.expr.graph", All),
        Import("de.sciss.lucre.swing.graph", All)
      ))
      //      proc.Code.registerImports(proc.Code.ActionRaw.id, Vec(
      //        Import("de.sciss.proc", Name("Widget") :: Nil)
      //      ))
    }

    // override because we need register imports
    override def init(): Unit = _init

    def mkCode(source: String): Repr = Code(source)
  }
  final case class Code(source: String) extends proc.Code {
    type In     = Unit
    type Out    = _Graph

    def tpe: proc.Code.Type = Code

    private def widgetCl = classOf[_Widget]

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = {
      CodeImpl.compileBody[In, Out, _Widget, Code](this, resCl = widgetCl)
    }

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
      Graph {
        CodeImpl.compileThunk[_Widget](this, resCl = widgetCl, execute = true)
      }

    def prelude : String =
      s"""object Main {
         |  def __result__ : ${widgetCl.getName} = {
         |""".stripMargin

    def postlude: String = "\n  }\n}\n"

    def updateSource(newText: String): Code = copy(source = newText)
  }
}
