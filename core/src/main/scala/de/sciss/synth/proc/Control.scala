/*
 *  Control.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.{Dummy, Event, EventLike, Publisher, Targets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.{Expr, Graph => _Graph}
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.model
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.proc
import de.sciss.synth.proc.Code.{Example, Import}
import de.sciss.synth.proc.impl.{CodeImpl, ControlImpl => Impl}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.Future

// I don't very much like the term 'control' and its cybernetic interpretation.
// What would be alternatives?
// - Script, Program, Snippet, Glue, Module, Machine, Actor, Agent, Mechanism, Device, Struct,
//   Assembly, Unit (this makes sense but shadows `scala.Unit`).
// None of them is without problems, so let's just stick to Control.
object Control extends Obj.Type {
  final val typeId = 0x1000F

  /** Source code of the graph function. */
  final val attrSource    = "graph-source"

  /** Boolean indicating whether view should go into edit mode by default. */
  final val attrEditMode  = "edit-mode"

  override def init(): Unit = {
    super   .init()
    GraphObj.init()
    Code    .init()
  }

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Control[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Control[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Control[S]] = Impl.serializer[S]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  type Graph = _Graph
  val Graph: _Graph.type = _Graph

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](w: Control[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  final case class GraphChange[S <: Sys[S]](change: model.Change[_Graph]) extends Change[S]

  // ---- Code ----

  object Code extends proc.Code.Type {
    final val id        = 7
    final val prefix    = "Control"
    final val humanName = "Control Graph"
    type Repr           = Code

    override def examples: ISeq[Example] = List(
      Example("Hello World", 'h',
        """val b = LoadBang()
          |b ---> PrintLn("Hello World!")
        """.stripMargin
      )
    )

//    override def defaultSource: String = s"${super.defaultSource}Empty()\n"

    def docBaseSymbol: String = "de.sciss.lucre.expr.graph"

    private[this] lazy val _init: Unit = {
      proc.Code.addType(this)
      import Import._
      proc.Code.registerImports(id, Vec(
        Import("de.sciss.numbers.Implicits", All),
        Import("de.sciss.lucre.expr.ExImport", All),
        Import("de.sciss.synth.proc.ExImport", All),
        Import("de.sciss.file", All),
        Import("de.sciss.lucre.expr.graph", All)
      ))
      proc.Code.registerImports(proc.Code.Action.id, Vec(
        Import("de.sciss.synth.proc", Name("Control") :: Nil)
      ))
    }

    // override because we need register imports
    override def init(): Unit = _init

    def mkCode(source: String): Repr = Code(source)
  }
  final case class Code(source: String) extends proc.Code {
    type In     = Unit
    type Out    = _Graph

    def tpe: proc.Code.Type = Code

//    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = {
//      import reflect.runtime.universe._
//      CodeImpl.compileBody[In, Out, _Control, Code](this, typeTag[_Control])
//    }
//
//    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
//      Graph {
//        import reflect.runtime.universe._
//        CodeImpl.compileThunk[_Control](this, typeTag[_Control], execute = true)
//      }
//
//    def prelude : String =
//      s"""object Main {
//         |  def __result__ : ${classOf[_Control].getName} = {
//         |""".stripMargin
//
//    def postlude: String = "\n  }\n}\n"

    def compileBody()(implicit compiler: proc.Code.Compiler): Future[Unit] = {
      import reflect.runtime.universe._
      CodeImpl.compileBody[In, Out, Unit, Code](this, typeTag[Unit])
    }

    def execute(in: In)(implicit compiler: proc.Code.Compiler): Out =
      Graph {
        import reflect.runtime.universe._
        CodeImpl.compileThunk[Unit](this, typeTag[Unit], execute = true)
      }

    def prelude : String = "object Main {\n"

    def postlude: String = "\n}\n"

    def updateSource(newText: String): Code = copy(source = newText)
  }

  // ---- graph obj ----

  object GraphObj extends expr.impl.ExprTypeImpl[_Graph, GraphObj] {
    final val typeId = 500

    protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                    (implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
      extends ConstImpl[S] with GraphObj[S]

    private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
      extends VarImpl[S] with GraphObj[S]

    /** A serializer for graphs. */
    def valueSerializer: ImmutableSerializer[_Graph] = _Graph.serializer

    private final val emptyCookie = 4

    override protected def readCookie[S <: Sys[S]](in: DataInput, access: S#Acc, cookie: Byte)
                                                  (implicit tx: S#Tx): _Ex[S] =
      cookie match {
        case `emptyCookie` =>
          val id = tx.readId(in, access)
          new Predefined(id, cookie)
        case _ => super.readCookie(in, access, cookie)
      }

    def empty[S <: Sys[S]](implicit tx: S#Tx): _Ex[S] = apply(emptyCookie)

    private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): _Ex[S] = {
      val id = tx.newId()
      new Predefined(id, cookie)
    }

    private final class Predefined[S <: Sys[S]](val id: S#Id, cookie: Int)
      extends GraphObj[S] with Expr.Const[S, _Graph] {

      def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

      def tpe: Obj.Type = GraphObj

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Predefined(txOut.newId(), cookie) // .connect()

      def write(out: DataOutput): Unit = {
        out.writeInt(tpe.typeId)
        out.writeByte(cookie)
        id.write(out)
      }

      def value(implicit tx: S#Tx): _Graph = constValue

      def changed: EventLike[S, model.Change[_Graph]] = Dummy[S, model.Change[_Graph]]

      def dispose()(implicit tx: S#Tx): Unit = ()

      def constValue: _Graph = cookie match {
        case `emptyCookie` => _Graph.empty // emptyGraph
      }
    }
  }
  trait GraphObj[S <: Sys[S]] extends Expr[S, _Graph]
}
trait Control[S <: Sys[S]] extends Obj[S] with Publisher[S, Control.Update[S]] {
  def graph: Control.GraphObj.Var[S]
}