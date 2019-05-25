/*
 *  Timeline.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.aux.{Aux, ProductWithAux}
import de.sciss.lucre.edit.EditTimeline
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{Caching, IEvent, ITargets}
import de.sciss.lucre.expr.graph.impl.{ExpandedObjMakeImpl, MappedIExpr, ObjCellViewImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction, IExpr, SpanLikeObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.model.Change
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}
import de.sciss.synth.proc

import scala.concurrent.stm.Ref

object Timeline {
  private lazy val _init: Unit =
    Aux.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Timeline] with Obj.Make[Timeline] = Apply()

  private[lucre] object Empty extends Timeline {
    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None
  }

  private final class ApplyExpanded[S <: Sys[S]](implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Timeline] {

    protected def empty: Timeline = Empty

    protected def make()(implicit tx: S#Tx): Timeline = {
      val peer = proc.Timeline[S]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Timeline] with Act with Obj.Make[Timeline] {
    override def productPrefix: String = "Timeline" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Timeline] with IAction[S]

    def make: Act = this

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded[S]
    }
  }

  private[lucre] def wrap[S <: Sys[S]](peer: stm.Source[S#Tx, proc.Timeline[S]], system: S): Timeline =
    new Impl[S](peer, system)

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, proc.Timeline[S]], system: S)
    extends ObjImplBase[S, proc.Timeline](in, system) with Timeline {

    override type Peer[~ <: Sys[~]] = proc.Timeline[~]
  }

  private final class CellViewImpl[S <: Sys[S]](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends ObjCellViewImpl[S, proc.Timeline, Timeline](h, key) {

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[proc.Timeline[S]]] =
      Serializer.option

    protected def lower(peer: proc.Timeline[S])(implicit tx: S#Tx): Timeline =
      new Impl(tx.newHandle(peer), tx.system)
  }

  implicit object Bridge extends Obj.Bridge[Timeline] with Aux.Factory {
    final val id = 2002

    type Repr[S <: Sys[S]] = proc.Timeline[S]

    def readIdentifiedAux(in: DataInput): Aux = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Timeline]] =
      new CellViewImpl(tx.newHandle(obj), key)
  }

  private final class AddExpanded[S <: Sys[S], A](in: IExpr[S, Timeline], span: IExpr[S, _SpanLike],
                                                  elem: IExpr[S, A])
                                                 (implicit source: Obj.Source[A])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit =
      for {
        tl  <- in.value.peer
        tlm <- tl.modifiableOption
      } {
        val spanV   = span.value
        val elemV   = elem.value
        val spanObj = SpanLikeObj.newVar[S](spanV)
        val elemObj = source.toObj(elemV)
        EditTimeline.add(tlm, spanObj, elemObj)
      }
  }

  final case class Add[A](in: Ex[Timeline], span: Ex[_SpanLike], elem: Ex[A])(implicit source: Obj.Source[A])
    extends Act with ProductWithAux {

    override def productPrefix: String = s"Timeline$$Add" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new AddExpanded(in.expand[S], span.expand[S], elem.expand[S])

    def aux: List[Aux] = source :: Nil
  }

  private type SplitPair = (Timed[Obj], Timed[Obj])

  private final class SplitExpanded[S <: Sys[S]](in: IExpr[S, Timeline], span: IExpr[S, _SpanLike],
                                                 elem: IExpr[S, Obj], time: IExpr[S, Long])
                                                (implicit protected val targets: ITargets[S])
    extends IAction[S] with IExpr[S, SplitPair]
      with IGenerator       [S, Change[SplitPair]]
      with ITriggerConsumer [S, Change[SplitPair]]
      with Caching {

    private def empty: SplitPair = (Timed(_Span.Void, Obj.Empty), Timed(_Span.Void, Obj.Empty))

    private[this] val ref = Ref[SplitPair](empty)

    def value(implicit tx: S#Tx): SplitPair = ref()

    def executeAction()(implicit tx: S#Tx): Unit =
      trigReceived() // .foreach(fire) --- we don't need to fire, there is nobody listening;

    private def findSpan(tl: proc.Timeline[S])(implicit tx: S#Tx): Option[SpanLikeObj[S]] = {
      val spanV = span.value

      ???
    }

    private def make()(implicit tx: S#Tx): SplitPair = {
      val opt = for {
        tl      <- in.value.peer
        tlm     <- tl.modifiableOption
        spanObj <- findSpan(tl)
        elemObj <- elem.value.peer[S]
      } yield {
        val timeV     = time.value
        val split     = EditTimeline.split(tlm, spanObj, elemObj, timeV)
        val leftObj   = Obj.wrap[S](tx.newHandle(split.leftObj  ), tx.system)
        val rightObj  = Obj.wrap[S](tx.newHandle(split.rightObj ), tx.system)
        val leftT     = Timed[Obj](split.leftSpan .value, leftObj )
        val rightT    = Timed[Obj](split.rightSpan.value, rightObj)
        (leftT, rightT)
      }
      opt.getOrElse(empty)
    }

    protected def trigReceived()(implicit tx: S#Tx): Option[Change[SplitPair]] = {
      val now     = make()
      val before  = ref.swap(now) // needs caching
      Some(Change(before, now))
    }

    def changed: IEvent[S, Change[SplitPair]] = this
  }

  object Split {
    // XXX TODO --- should have tuple ._1 and ._2 off the shelf
    private final class LeftExpanded[S <: Sys[S], A](in: IExpr[S, (Timed[Obj], Timed[Obj])], tx0: S#Tx)
                                                     (implicit targets: ITargets[S])
      extends MappedIExpr[S, (Timed[Obj], Timed[Obj]), Timed[Obj]](in, tx0) {

      protected def mapValue(inValue: (Timed[Obj], Timed[Obj]))(implicit tx: S#Tx): Timed[Obj] = inValue._1
    }

    final case class Left(s: Split) extends Ex[Timed[Obj]] {
      override def productPrefix: String = s"Timeline$$Split$$Left" // serialization

      type Repr[S <: Sys[S]] = IExpr[S, Timed[Obj]]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new LeftExpanded(s.expand[S], tx)
      }
    }

    // XXX TODO --- should have tuple ._1 and ._2 off the shelf
    private final class RightExpanded[S <: Sys[S], A](in: IExpr[S, (Timed[Obj], Timed[Obj])], tx0: S#Tx)
                                                    (implicit targets: ITargets[S])
      extends MappedIExpr[S, (Timed[Obj], Timed[Obj]), Timed[Obj]](in, tx0) {

      protected def mapValue(inValue: (Timed[Obj], Timed[Obj]))(implicit tx: S#Tx): Timed[Obj] = inValue._2
    }

    final case class Right(s: Split) extends Ex[Timed[Obj]] {
      override def productPrefix: String = s"Timeline$$Split$$Right" // serialization

      type Repr[S <: Sys[S]] = IExpr[S, Timed[Obj]]

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new RightExpanded(s.expand[S], tx)
      }
    }
  }
  final case class Split(in: Ex[Timeline], span: Ex[_SpanLike], elem: Ex[Obj], time: Ex[Long])
    extends Act {

    override def productPrefix: String = s"Timeline$$Split" // serialization

    type Repr[S <: Sys[S]] = IAction[S] with IExpr[S, (Timed[Obj], Timed[Obj])]

    def left  : Ex[Timed[Obj]] = Split.Left (this)
    def right : Ex[Timed[Obj]] = Split.Right(this)

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new SplitExpanded(in.expand[S], span.expand[S], elem.expand[S], time.expand[S])
    }
  }

  implicit final class Ops(private val tl: Ex[Timeline]) extends AnyVal {
    def add [A](span: Ex[_SpanLike], elem: Ex[A])(implicit source: Obj.Source[A]): Act = Add(tl, span, elem)
    def +=  [A](tup: (Ex[_SpanLike], Ex[A]))     (implicit source: Obj.Source[A]): Act = add(tup._1, tup._2)

    def split(span: Ex[_SpanLike], elem: Ex[Obj], time: Ex[Long]): Split =
      Split(tl, span, elem, time)
  }
}
trait Timeline extends Obj {
  type Peer[~ <: Sys[~]] = proc.Timeline[~]
}