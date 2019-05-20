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
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.impl.{ExpandedObjMakeImpl, ObjImplBase}
import de.sciss.lucre.expr.impl.{IActionImpl, CellViewImpl => _CellViewImpl}
import de.sciss.lucre.expr.{CellView, Context, IAction, IExpr, SpanLikeObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.span.SpanLike
import de.sciss.synth.proc

import scala.concurrent.stm.Ref

object Timeline {
  private lazy val _init: Unit =
    Aux.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Timeline] with Obj.Make[Timeline] = Apply()

  private object Empty extends Timeline {
    private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None
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

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, proc.Timeline[S]], system: S)
    extends ObjImplBase[S, proc.Timeline](in, system) with Timeline {

    override type Peer[~ <: Sys[~]] = proc.Timeline[~]
  }

  private final class CellViewImpl[S <: Sys[S]](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends CellView.Var[S, Option[Timeline]] with _CellViewImpl.Basic[S#Tx, Option[Timeline]] {

    type Repr = Option[proc.Timeline[S]]

    def repr(implicit tx: S#Tx): Option[proc.Timeline[S]] =
      h().attr.$[proc.Timeline](key)

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[proc.Timeline[S]]] =
      Serializer.option

    def repr_=(value: Option[proc.Timeline[S]])(implicit tx: S#Tx): Unit = {
      val a = h().attr
      value match {
        case Some(f)  => a.put(key, f)
        case None     => a.remove(key)
      }
    }

    def lift(value: Option[Timeline])(implicit tx: S#Tx): Option[proc.Timeline[S]] =
      value.flatMap(_.peer)

    def apply()(implicit tx: S#Tx): Option[Timeline] = repr.map(lower)

    private def lower(peer: proc.Timeline[S])(implicit tx: S#Tx): Timeline =
      new Impl(tx.newHandle(peer), tx.system)

    def update(v: Option[Timeline])(implicit tx: S#Tx): Unit = {
      val peer = v.flatMap(_.peer)
      repr = peer
    }

    def react(fun: S#Tx => Option[Timeline] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      new Observation[S](h().attr, key, fun, tx)
  }

  private final class Observation[S <: Sys[S]](attr: AttrMap[S], key: String, fun: S#Tx => Option[Timeline] => Unit,
                                               tx0: S#Tx) extends Disposable[S#Tx] {
    private[this] val ref = Ref(Option.empty[(proc.Timeline[S], Disposable[S#Tx])])

    private def mkFObs(f: proc.Timeline[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      f.changed.react { implicit tx => _ =>
        val ex = lower(f)
        fun(tx)(Some(ex))
      }

    private def setObj(fOpt: Option[proc.Timeline[S]])(implicit tx: S#Tx): Boolean =
      (ref(), fOpt) match {
        case (None, Some(fNew))  =>
          val newObs = mkFObs(fNew)
          ref() = Some((fNew, newObs))
          true
        case (Some((_, oldObs)), None) =>
          oldObs.dispose()
          ref() = None
          true
        case (Some((fOld, oldObs)), Some(fNew)) if fOld != fNew =>
          val newObs = mkFObs(fNew)
          ref() = Some((fNew, newObs))
          oldObs.dispose()
          true
        case _ => false
      }

    private def setObjAndFire(fOpt: Option[proc.Timeline[S]])(implicit tx: S#Tx): Unit =
      if (setObj(fOpt)) fun(tx)(fOpt.map(lower))

    private def init()(implicit tx: S#Tx): Unit =
      setObj(attr.$[proc.Timeline](key))

    init()(tx0)

    private def lower(peer: proc.Timeline[S])(implicit tx: S#Tx): Timeline =
      new Impl(tx.newHandle(peer), tx.system)

    private[this] val attrObs = attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case stm.Obj.AttrAdded    (`key`    , f: proc.Timeline[S]) => setObjAndFire(Some(f))
        case stm.Obj.AttrAdded    (`key`    , _)                => setObjAndFire(None)
        case stm.Obj.AttrRemoved  (`key`    , _: proc.Timeline[S]) => setObjAndFire(None)
        case stm.Obj.AttrReplaced (`key`, _ , f: proc.Timeline[S]) => setObjAndFire(Some(f))
        case stm.Obj.AttrReplaced (`key`, _ , _)                => setObjAndFire(None)
        case _ =>
      }
    } (tx0)

    def dispose()(implicit tx: S#Tx): Unit = {
      attrObs.dispose()
      ref.swap(None).foreach(_._2.dispose())
    }
  }

  implicit object Bridge extends Obj.Bridge[Timeline] with Aux.Factory {
    final val id = 2002

    type Repr[S <: Sys[S]] = proc.Timeline[S]

    def readIdentifiedAux(in: DataInput): Aux = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Timeline]] =
      new CellViewImpl(tx.newHandle(obj), key)
  }

//  // XXX TODO DRY with Folder.ExpandedImpl
//  private abstract class ExpandedImpl[S <: Sys[S], A](in: IExpr[S, Timeline], init: A, tx0: S#Tx)
//                                                     (implicit protected val targets: ITargets[S])
//    extends IExpr[S, A] with IGenerator[S, Change[A]] with Caching {
//
//    private[this] val obs   = Ref[Disposable[S#Tx]](Disposable.empty)
//    private[this] val cache = Ref(init)
//
//    protected def mapValue(t: BiGroup[S, stm.Obj[S]])(implicit tx: S#Tx): A
//
//    private def setObj(v: Timeline)(implicit tx: S#Tx): Option[Change[A]] = {
//      obs.swap(Disposable.empty).dispose()
//      // XXX TODO --- should we also fire when size has been non-zero and v.peer is empty?
//      v.peer.flatMap { f =>
//        val newObs = f.changed.react { implicit tx => upd =>
//          val now     = mapValue(upd.group)
//          val before  = cache.swap(now)
//          if (before != now) fire(Change(before, now))
//        }
//        obs() = newObs
//        val now     = mapValue(f)
//        val before  = cache.swap(now)
//        if (before != now) Some(Change(before, now)) else None
//      }
//    }
//
//    in.changed.--->(this)(tx0)
//    setObj(in.value(tx0))(tx0)
//
//    def value(implicit tx: S#Tx): A = cache()
//
//    def changed: IEvent[S, Change[A]] = this
//
//    def dispose()(implicit tx: S#Tx): Unit = {
//      in.changed.-/->(this)
//      obs.swap(Disposable.empty).dispose()
//    }
//
//    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
//      if (pull.isOrigin(this)) Some(pull.resolve)
//      else {
//        pull(in.changed).flatMap { ch =>
//          setObj(ch.now)
//        }
//      }
//  }

  private final class AddExpanded[S <: Sys[S], A](in: IExpr[S, Timeline], span: IExpr[S, SpanLike], elem: IExpr[S, A])
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
        tlm.add(spanObj, elemObj)
      }
  }

  final case class Add[A](in: Ex[Timeline], span: Ex[SpanLike], elem: Ex[A])(implicit source: Obj.Source[A])
    extends Act with ProductWithAux {

    override def productPrefix: String = s"Timeline$$Put" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new AddExpanded(in.expand[S], span.expand[S], elem.expand[S])

    def aux: List[Aux] = source :: Nil
  }


  implicit final class Ops(private val tl: Ex[Timeline]) extends AnyVal {
    def add [A](span: Ex[SpanLike], elem: Ex[A])(implicit source: Obj.Source[A]): Act = Add(tl, span, elem)
    def +=  [A](tup: (Ex[SpanLike], Ex[A]))     (implicit source: Obj.Source[A]): Act = add(tup._1, tup._2)
  }
}
trait Timeline extends Obj {
  type Peer[~ <: Sys[~]] = proc.Timeline[~]
}