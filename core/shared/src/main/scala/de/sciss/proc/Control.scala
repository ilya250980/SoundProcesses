/*
 *  Control.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.expr.{Graph => _Graph}
import de.sciss.lucre.impl.{DummyEvent, ExprTypeImpl}
import de.sciss.lucre.{Copy, Elem, Event, EventLike, Expr, Ident, Obj, Publisher, Txn, Var => LVar}
import de.sciss.model
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, TFormat}
import de.sciss.synth.UGenSource.Vec
import de.sciss.proc.impl.{ControlImpl => Impl}

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

  def apply[T <: Txn[T]]()(implicit tx: T): Control[T] = Impl[T]()

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Control[T] = Impl.read(in)

  implicit def format[T <: Txn[T]]: TFormat[T, Control[T]] = Impl.format[T]

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

  type Graph = _Graph
  val Graph: _Graph.type = _Graph

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[T <: Txn[T]](w: Control[T], changes: Vec[Change[T]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[T <: Txn[T]]

  final case class GraphChange[T <: Txn[T]](change: model.Change[_Graph]) extends Change[T]

  // ---- graph obj ----

  object GraphObj extends ExprTypeImpl[_Graph, GraphObj] {
    final val typeId = 500

    def tryParse(value: Any): Option[_Graph] = value match {
      case x: _Graph  => Some(x)
      case _          => None
    }

    protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
      new _Const[T](id, value)

    protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                    (implicit tx: T): Var[T] = {
      val res = new _Var[T](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
      extends ConstImpl[T] with GraphObj[T]

    private final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
      extends VarImpl[T] with GraphObj[T]

    /** A format for graphs. */
    def valueFormat: ConstFormat[_Graph] = _Graph.format

    private final val emptyCookie = 4

    override protected def readCookie[T <: Txn[T]](in: DataInput, cookie: Byte)(implicit tx: T): E[T] =
      cookie match {
        case `emptyCookie` =>
          val id = tx.readId(in)
          new Predefined(id, cookie)
        case _ => super.readCookie(in, cookie)
      }

    def empty[T <: Txn[T]](implicit tx: T): E[T] = apply(emptyCookie)

    private def apply[T <: Txn[T]](cookie: Int)(implicit tx: T): E[T] = {
      val id = tx.newId()
      new Predefined(id, cookie)
    }

    private final class Predefined[T <: Txn[T]](val id: Ident[T], cookie: Int)
      extends GraphObj[T] with Expr.Const[T, _Graph] {

      def event(slot: Int): Event[T, Any] = throw new UnsupportedOperationException

      def tpe: Obj.Type = GraphObj

      def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
        new Predefined(txOut.newId(), cookie) // .connect()

      def write(out: DataOutput): Unit = {
        out.writeInt(tpe.typeId)
        out.writeByte(cookie)
        id.write(out)
      }

      def value(implicit tx: T): _Graph = constValue

      def changed: EventLike[T, model.Change[_Graph]] = DummyEvent()

      def dispose()(implicit tx: T): Unit = ()

      def constValue: _Graph = cookie match {
        case `emptyCookie` => _Graph.empty // emptyGraph
      }
    }
  }
  trait GraphObj[T <: Txn[T]] extends Expr[T, _Graph]
}
trait Control[T <: Txn[T]] extends Obj[T] with Publisher[T, Control.Update[T]] {
  def graph: Control.GraphObj.Var[T]
}