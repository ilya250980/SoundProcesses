/*
 *  Action.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.{Dummy, Event, EventLike, Publisher, Targets}
import de.sciss.lucre.expr
import de.sciss.lucre.expr.graph.{Act, Control => _Control}
import de.sciss.lucre.expr.impl.{ExElem, GraphBuilderMixin, GraphSerializerMixin}
import de.sciss.lucre.expr.{Context, Expr, IAction, IControl, ITrigger}
import de.sciss.lucre.stm.{Copy, Elem, Folder, Obj, Sys}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer}
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{ActionRawImpl, ActionImpl => Impl}
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

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Action[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action[S] = Impl.read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action[S]] = Impl.serializer[S]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[S <: Sys[S]](w: Action[S], changes: Vec[Change[S]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[S <: Sys[S]]

  final case class GraphChange[S <: Sys[S]](change: model.Change[Graph]) extends Change[S]

  // ---- graph obj ----

  object GraphObj extends expr.impl.ExprTypeImpl[Graph, GraphObj] {
    final val typeId = 600

    def tryParse(value: Any): Option[Graph] = value match {
      case x: Graph => Some(x)
      case _        => None
    }

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
    def valueSerializer: ImmutableSerializer[Graph] = Graph.serializer

    private final val emptyCookie = 4

    override protected def readCookie[S <: Sys[S]](in: DataInput, access: S#Acc, cookie: Byte)
                                                  (implicit tx: S#Tx): _Ex[S] =
      cookie match {
        case `emptyCookie` =>
          val id = tx.readId(in, access)
          new Predefined(id, cookie)
        case _ => super.readCookie(in, access, cookie)
      }

    private val emptyGraph =
      Graph {
        import lucre.expr.graph._
        Act.Nop()
      }

    def empty[S <: Sys[S]](implicit tx: S#Tx): _Ex[S] = apply(emptyCookie)

    private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): _Ex[S] = {
      val id = tx.newId()
      new Predefined(id, cookie)
    }

    private final class Predefined[S <: Sys[S]](val id: S#Id, cookie: Int)
      extends GraphObj[S] with Expr.Const[S, Graph] {

      def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

      def tpe: Obj.Type = GraphObj

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Predefined(txOut.newId(), cookie) // .connect()

      def write(out: DataOutput): Unit = {
        out.writeInt(tpe.typeId)
        out.writeByte(cookie)
        id.write(out)
      }

      def value(implicit tx: S#Tx): Graph = constValue

      def changed: EventLike[S, model.Change[Graph]] = Dummy[S, model.Change[Graph]]

      def dispose()(implicit tx: S#Tx): Unit = ()

      def constValue: Graph = cookie match {
        case `emptyCookie` => emptyGraph
      }
    }
  }
  trait GraphObj[S <: Sys[S]] extends Expr[S, Graph]

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

    implicit object serializer extends ImmutableSerializer[Graph] with GraphSerializerMixin {
      private final val SER_VERSION = 0x4147  // "AG"

      def write(g: Graph, out: DataOutput): Unit = {
        out.writeShort(SER_VERSION)
        var ref = null: ExElem.RefMapOut
        ref = ExElem.write(g.action, out, ref)
        val cx = g.controls
        writeControls(cx, out, ref)
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

    private final class ExpandedImpl[S <: Sys[S]](val action: IAction[S], controls: ISeq[IControl[S]])
      extends IAction[S] with IControl[S] {

      def initControl()(implicit tx: S#Tx): Unit =
        controls.foreach(_.initControl())

      def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit =
        action.addSource(tr)

      def executeAction()(implicit tx: S#Tx): Unit =
        action.executeAction()

      def dispose()(implicit tx: S#Tx): Unit = {
        action.dispose()
        controls.foreach(_.dispose())
      }
    }
  }

  final case class Graph(action: Act, controls: Vec[_Control.Configured])
    extends expr.Graph {

    override def expand[S <: Sys[S]](implicit tx: S#Tx, ctx: Context[S]): IAction[S] with IControl[S] = {
      ctx.initGraph(this)
      val actionEx: IAction[S] /*with IControl[S]*/ = action.expand[S]
      val disposables = controls.map(_.control.expand[S])
      new Graph.ExpandedImpl(actionEx, disposables)
    }
  }

  // ---- LEGACY ----

  object Universe {
    @deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
    def apply[S <: Sys[S]](self: ActionRaw[S], invoker: Option[Obj[S]] = None, value: Any = ())
                          (implicit peer: proc.Universe[S]): Universe[S] =
      new ActionRawImpl.UniverseImpl(self, invoker, value)
  }

  /** Environment passed into the action body. Deliberately not a sub-type of `proc.Universe`,
    * but carrying over some of the same methods.
    */
  @deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
  trait Universe[S <: Sys[S]] extends proc.Universe.Base[S] {
    implicit def peer: proc.Universe[S]

    /** The action object itself, most prominently giving access to
      * linked objects via its attributes.
      */
    def self: ActionRaw[S]

    /** A result object from the invoker. To permit different kind of invocations,
      * this value is untyped. Conventionally, `Action.DoubleVector` and `Action.FloatVector`
      * are used for collections of numbers.
      */
    def value: Any

    /** Parent component from which the action is invoked. For example
      * if used from within a synth-graph, this will be some `Proc.Obj`.
      * `None` if the action is directly invoked without dedicated parent.
      */
    def invoker: Option[Obj[S]]

    def root(implicit tx: S#Tx): Folder[S] = workspace.root

    def log(what: => String)(implicit tx: S#Tx): Unit
  }

  @deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
  trait Body {
    def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit
  }
}
trait Action[S <: Sys[S]] extends Obj[S] with Publisher[S, Action.Update[S]] {
  def graph: Action.GraphObj.Var[S]
}