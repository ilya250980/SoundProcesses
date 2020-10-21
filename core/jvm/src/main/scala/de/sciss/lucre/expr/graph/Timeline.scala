/*
 *  Timeline.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.edit.EditTimeline
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, MappedIExpr, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, ITriggerConsumer}
import de.sciss.lucre.expr.{CellView, Context, IAction}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Adjunct, Caching, IChangeEvent, IExpr, IPush, ITargets, ProductWithAdjuncts, Source, SpanLikeObj, Sys, Txn, Obj => LObj}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.span.{Span => _Span, SpanLike => _SpanLike}
import de.sciss.synth.proc

import scala.concurrent.stm.Ref

object Timeline {
  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Timeline] with Obj.Make = Apply()

  private[lucre] object Empty extends Timeline {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] = None

    override def toString: String = "Timeline<empty>"
  }

  private final class ApplyExpanded[T <: Txn[T]](implicit targets: ITargets[T])
    extends ExpandedObjMakeImpl[T, Timeline] {

    protected def empty: Timeline = Empty

    protected def make()(implicit tx: T): Timeline = {
      val peer = proc.Timeline[T]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Timeline] with Act with Obj.Make {
    override def productPrefix: String = "Timeline" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Timeline] with IAction[T]

    def make: Act = this

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ApplyExpanded[T]
    }
  }

  // used by Mellite (no transaction available)
  private[lucre] def wrapH[T <: Txn[T]](peer: Source[T, proc.Timeline[T]], system: Sys): Timeline =
    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: proc.Timeline[T])(implicit tx: T): Timeline =
    new Impl[T](tx.newHandle(peer), tx.system)

  private final class Impl[T <: Txn[T]](in: Source[T, proc.Timeline[T]], system: Sys)
    extends ObjImplBase[T, proc.Timeline](in, system) with Timeline {

    override type Peer[~ <: Txn[~]] = proc.Timeline[~]
  }

  private final class CellViewImpl[T <: Txn[T]](h: Source[T, LObj[T]], key: String)
    extends ObjCellViewVarImpl[T, proc.Timeline, Timeline](h, key) {

    implicit def format: TFormat[T, Option[proc.Timeline[T]]] =
      TFormat.option

    protected def lower(peer: proc.Timeline[T])(implicit tx: T): Timeline =
      wrap(peer)
  }

  implicit object Bridge extends Obj.Bridge[Timeline] with HasDefault[Timeline] with Adjunct.Factory {
    final val id = 2002

    type Repr[T <: Txn[T]] = proc.Timeline[T]

    def defaultValue: Timeline = Empty

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Timeline]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Timeline]] =
      new AbstractCtxCellView[T, Timeline](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[Timeline] = value match {
          case tl: Timeline => Some(tl)
          case _            => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[Timeline] = obj match {
          case peer: proc.Timeline[T] => Some(wrap(peer))
          case _                      => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Timeline] =
      obj.attr.$[proc.Timeline](key).map(wrap(_))

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Timeline] = obj match {
      case a: proc.Timeline[T]  => Some(wrap(a))
      case _                    => None
    }
  }

  private final class AddExpanded[T <: Txn[T], A](in: IExpr[T, Timeline], span: IExpr[T, _SpanLike],
                                                  elem: IExpr[T, A])
                                                 (implicit source: Obj.Source[A])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      for {
        tl  <- in.value.peer
        tlm <- tl.modifiableOption
      } {
        val spanV   = span.value
        val elemV   = elem.value
        val spanObj = SpanLikeObj.newVar[T](spanV)
        val elemObj = source.toObj(elemV)
        EditTimeline.add(tlm, spanObj, elemObj)
      }
  }

  final case class Add[A](in: Ex[Timeline], span: Ex[_SpanLike], elem: Ex[A])(implicit source: Obj.Source[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Timeline$$Add" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new AddExpanded(in.expand[T], span.expand[T], elem.expand[T])

    override def adjuncts: List[Adjunct] = source :: Nil
  }

  private final class AddAllExpanded[T <: Txn[T], A](in: IExpr[T, Timeline], pairs: IExpr[T, Seq[(_SpanLike, A)]])
                                                 (implicit source: Obj.Source[A])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      for {
        tl  <- in.value.peer
        tlm <- tl.modifiableOption
      } {
        val pairsV  = pairs.value
        pairsV.foreach { case (spanV, elemV) =>
          val spanObj = SpanLikeObj.newVar[T](spanV)
          val elemObj = source.toObj(elemV)
          EditTimeline.add(tlm, spanObj, elemObj)
        }
      }
  }

  final case class AddAll[A](in: Ex[Timeline], pairs: Ex[Seq[(_SpanLike, A)]])(implicit source: Obj.Source[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Timeline$$AddAll" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new AddAllExpanded(in.expand[T], pairs.expand[T])

    override def adjuncts: List[Adjunct] = source :: Nil
  }

  private final class RemoveExpanded[T <: Txn[T]](in: IExpr[T, Timeline], span: IExpr[T, _SpanLike],
                                                  elem: IExpr[T, Obj])
    extends IActionImpl[T] {

    private def findSpan(tl: proc.Timeline[T], elemObj: LObj[T])(implicit tx: T): Option[SpanLikeObj[T]] = {
      val spanV = span.value
      tl.recoverSpan(spanV, elemObj)
    }

    def executeAction()(implicit tx: T): Unit =
      for {
        tl      <- in.value.peer
        tlm     <- tl.modifiableOption
        elemObj <- elem.value.peer[T]
        spanObj <- findSpan(tl, elemObj)
      } {
        EditTimeline.unlinkAndRemove(tlm, spanObj, elemObj)
      }
  }

  final case class Remove(in: Ex[Timeline], span: Ex[_SpanLike], elem: Ex[Obj])
    extends Act {

    override def productPrefix: String = s"Timeline$$Remove" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new RemoveExpanded(in.expand[T], span.expand[T], elem.expand[T])
  }

  private type SplitPair = (Timed[Obj], Timed[Obj])

  private final class SplitExpanded[T <: Txn[T]](in: IExpr[T, Timeline], span: IExpr[T, _SpanLike],
                                                 elem: IExpr[T, Obj], time: IExpr[T, Long])
                                                (implicit protected val targets: ITargets[T])
    extends IAction[T] with IExpr[T, SplitPair]
      with IChangeGeneratorEvent [T, SplitPair]
      with ITriggerConsumer      [T, SplitPair]
      with Caching {

    private def empty: SplitPair = (Timed(_Span.Void, Obj.Empty), Timed(_Span.Void, Obj.Empty))

    private[this] val ref = Ref[SplitPair](empty)

    def value(implicit tx: T): SplitPair =
      IPush.tryPull(this).fold(ref())(_.now)

    def executeAction()(implicit tx: T): Unit =
      trigReceived() // .foreach(fire) --- we don't need to fire, there is nobody listening;

    private def findSpan(tl: proc.Timeline[T], elemObj: LObj[T])(implicit tx: T): Option[SpanLikeObj[T]] = {
      val spanV = span.value
      tl.recoverSpan(spanV, elemObj)
    }

    private def make()(implicit tx: T): SplitPair = {
      val opt = for {
        tl      <- in.value.peer
        tlm     <- tl.modifiableOption
        elemObj <- elem.value.peer[T]
        spanObj <- findSpan(tl, elemObj)
      } yield {
        val timeV     = time.value
        val split     = EditTimeline.split(tlm, spanObj, elemObj, timeV)
        val leftObj   = Obj.wrap[T](split.leftObj )
        val rightObj  = Obj.wrap[T](split.rightObj)
        val leftT     = Timed[Obj](split.leftSpan .value, leftObj )
        val rightT    = Timed[Obj](split.rightSpan.value, rightObj)
        (leftT, rightT)
      }
      opt.getOrElse(empty)
    }

    protected def valueBefore ()(implicit tx: T): SplitPair = ref()

    protected def trigReceived()(implicit tx: T): SplitPair = {
      val now = make()
      ref()   = now
      now
    }

    def changed: IChangeEvent[T, SplitPair] = this
  }

  object Split {
    // XXX TODO --- should have tuple ._1 and ._2 off the shelf
    private final class LeftExpanded[T <: Txn[T], A](in: IExpr[T, (Timed[Obj], Timed[Obj])], tx0: T)
                                                     (implicit targets: ITargets[T])
      extends MappedIExpr[T, (Timed[Obj], Timed[Obj]), Timed[Obj]](in, tx0) {

      protected def mapValue(inValue: (Timed[Obj], Timed[Obj]))(implicit tx: T): Timed[Obj] = inValue._1
    }

    final case class Left(s: Split) extends Ex[Timed[Obj]] {
      override def productPrefix: String = s"Timeline$$Split$$Left" // serialization

      type Repr[T <: Txn[T]] = IExpr[T, Timed[Obj]]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new LeftExpanded(s.expand[T], tx)
      }
    }

    // XXX TODO --- should have tuple ._1 and ._2 off the shelf
    private final class RightExpanded[T <: Txn[T], A](in: IExpr[T, (Timed[Obj], Timed[Obj])], tx0: T)
                                                    (implicit targets: ITargets[T])
      extends MappedIExpr[T, (Timed[Obj], Timed[Obj]), Timed[Obj]](in, tx0) {

      protected def mapValue(inValue: (Timed[Obj], Timed[Obj]))(implicit tx: T): Timed[Obj] = inValue._2
    }

    final case class Right(s: Split) extends Ex[Timed[Obj]] {
      override def productPrefix: String = s"Timeline$$Split$$Right" // serialization

      type Repr[T <: Txn[T]] = IExpr[T, Timed[Obj]]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new RightExpanded(s.expand[T], tx)
      }
    }
  }
  final case class Split(in: Ex[Timeline], span: Ex[_SpanLike], elem: Ex[Obj], time: Ex[Long])
    extends Act {

    override def productPrefix: String = s"Timeline$$Split" // serialization

    type Repr[T <: Txn[T]] = IAction[T] with IExpr[T, (Timed[Obj], Timed[Obj])]

    def left  : Ex[Timed[Obj]] = Split.Left (this)
    def right : Ex[Timed[Obj]] = Split.Right(this)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new SplitExpanded(in.expand[T], span.expand[T], elem.expand[T], time.expand[T])
    }
  }

  implicit final class Ops(private val tl: Ex[Timeline]) extends AnyVal {
    def add   [A](span: Ex[_SpanLike], elem: Ex[A  ])(implicit source: Obj.Source[A]): Act   = Add   (tl, span, elem)
    def remove   (span: Ex[_SpanLike], elem: Ex[Obj])                                : Act   = Remove(tl, span, elem)
    def split    (span: Ex[_SpanLike], elem: Ex[Obj], time: Ex[Long])                : Split = Split (tl, span, elem, time)

    def addAll[A](pairs: Ex[Seq[(_SpanLike, A)]])(implicit source: Obj.Source[A]): Act = AddAll(tl, pairs)
  }
}
trait Timeline extends Obj {
  type Peer[~ <: Txn[~]] = proc.Timeline[~]
}