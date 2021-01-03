/*
 *  Action.scala
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
import de.sciss.lucre.expr.graph.{Act, Control => _Control}
import de.sciss.lucre.expr.impl.{ExElem, GraphBuilderMixin, GraphFormatMixin}
import de.sciss.lucre.expr.{Context, IAction, IControl, ITrigger}
import de.sciss.lucre.impl.{DummyEvent, ExprTypeImpl}
import de.sciss.lucre.{Copy, Elem, Event, EventLike, Expr, Ident, Obj, Publisher, Txn, expr, Var => LVar}
import de.sciss.proc.impl.{ActionImpl => Impl}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, TFormat}
import de.sciss.synth.UGenSource.Vec
import de.sciss.{lucre, model}

import scala.collection.immutable.{Seq => ISeq}

// XXX TODO --- complete DRY with Control
object Action extends Obj.Type {
  final val typeId = 0x10010

  /** Source code of the graph function. */
  final val attrSource    = "graph-source"

  override def init(): Unit = {
    super   .init()
    GraphObj.init()
    Code    .init()
  }

  def apply[T <: Txn[T]]()(implicit tx: T): Action[T] = Impl[T]()

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Action[T] = Impl.read(in)

  implicit def format[T <: Txn[T]]: TFormat[T, Action[T]] = Impl.format[T]

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[T <: Txn[T]](w: Action[T], changes: Vec[Change[T]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[T <: Txn[T]]

  final case class GraphChange[T <: Txn[T]](change: model.Change[Graph]) extends Change[T]

  // ---- graph obj ----

  object GraphObj extends ExprTypeImpl[Graph, GraphObj] {
    final val typeId = 600

    def tryParse(value: Any): Option[Graph] = value match {
      case x: Graph => Some(x)
      case _        => None
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
    def valueFormat: ConstFormat[Graph] = Graph.format

    private final val emptyCookie = 4

    override protected def readCookie[T <: Txn[T]](in: DataInput, cookie: Byte)(implicit tx: T): E[T] =
      cookie match {
        case `emptyCookie` =>
          val id = tx.readId(in)
          new Predefined(id, cookie)
        case _ => super.readCookie(in, cookie)
      }

    private val emptyGraph =
      Graph {
        import lucre.expr.graph._
        Act.Nop()
      }

    def empty[T <: Txn[T]](implicit tx: T): E[T] = apply(emptyCookie)

    private def apply[T <: Txn[T]](cookie: Int)(implicit tx: T): E[T] = {
      val id = tx.newId()
      new Predefined(id, cookie)
    }

    private final class Predefined[T <: Txn[T]](val id: Ident[T], cookie: Int)
      extends GraphObj[T] with Expr.Const[T, Graph] {

      def event(slot: Int): Event[T, Any] = throw new UnsupportedOperationException

      def tpe: Obj.Type = GraphObj

      def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
        new Predefined(txOut.newId(), cookie) // .connect()

      def write(out: DataOutput): Unit = {
        out.writeInt(tpe.typeId)
        out.writeByte(cookie)
        id.write(out)
      }

      def value(implicit tx: T): Graph = constValue

      def changed: EventLike[T, model.Change[Graph]] = DummyEvent()

      def dispose()(implicit tx: T): Unit = ()

      def constValue: Graph = cookie match {
        case `emptyCookie` => emptyGraph
      }
    }
  }
  trait GraphObj[T <: Txn[T]] extends Expr[T, Graph]

  // ---- Graph ----

  object Graph {
    type Builder = expr.Graph.Builder

    def apply(thunk: => Act): Graph = {
      val b = new BuilderImpl
      use(b) {
        val w = thunk
        b.build(w)
      }
    }

    def use[A](b: Builder)(body: => A): A = expr.Graph.use(b)(body)

    def builder: Builder = expr.Graph.builder

    private[this] final class BuilderImpl extends GraphBuilderMixin {
      override def toString = s"lucre.swing.Graph.Builder@${hashCode.toHexString}"

      def build(w: Act): Graph = {
        val configured = buildControls()
        Graph(w, configured)
      }
    }

    implicit object format extends ConstFormat[Graph] with GraphFormatMixin {
      private final val SER_VERSION = 0x4147  // "AG"

      def write(g: Graph, out: DataOutput): Unit = {
        out.writeShort(SER_VERSION)
        var ref = null: ExElem.RefMapOut
        ref = ExElem.write(g.action, out, ref)
        val cx = g.controls
        writeControls(cx, out, ref)
        ()
      }

      def read(in: DataInput): Graph = {
        val cookie = in.readShort()
        require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
        val ref = new ExElem.RefMapIn
        val w   = ExElem.read (in, ref).asInstanceOf[Act]
        val cx  = readControls(in, ref)
        Graph(w, cx)
      }
    }

    private final class ExpandedImpl[T <: Txn[T]](val action: IAction[T], controls: ISeq[IControl[T]])
      extends IAction[T] with IControl[T] {

      def initControl()(implicit tx: T): Unit =
        controls.foreach(_.initControl())

      def addSource(tr: ITrigger[T])(implicit tx: T): Unit =
        action.addSource(tr)

      def executeAction()(implicit tx: T): Unit =
        action.executeAction()

      def dispose()(implicit tx: T): Unit = {
        action.dispose()
        controls.foreach(_.dispose())
      }
    }
  }

  final case class Graph(action: Act, controls: Vec[_Control.Configured])
    extends expr.Graph {

    override def expand[T <: Txn[T]](implicit tx: T, ctx: Context[T]): IAction[T] with IControl[T] = {
      ctx.initGraph(this)
      val actionEx: IAction[T] /*with IControl[T]*/ = action.expand[T]
      val disposables = controls.map(_.control.expand[T])
      new Graph.ExpandedImpl(actionEx, disposables)
    }
  }
}
trait Action[T <: Txn[T]] extends Obj[T] with Publisher[T, Action.Update[T]] {
  def graph: Action.GraphObj.Var[T]
}