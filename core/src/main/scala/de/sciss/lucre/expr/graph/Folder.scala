/*
 *  Folder.scala
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
import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{Caching, IEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.impl.ObjImplBase
import de.sciss.lucre.expr.impl.{IActionImpl, CellViewImpl => _CellViewImpl}
import de.sciss.lucre.expr.{CellView, Context, IAction, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj.AttrMap
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, Serializer}

import scala.concurrent.stm.Ref

object Folder {
  private lazy val _init: Unit =
    Aux.addFactory(Bridge)

  def init(): Unit = _init

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, stm.Folder[S]], system: S)
    extends ObjImplBase[S, stm.Folder](in, system) with Folder {

    override type Peer[~ <: Sys[~]] = stm.Folder[~]
  }

  private final class CellViewImpl[S <: Sys[S]](h: stm.Source[S#Tx, stm.Obj[S]], key: String)
    extends CellView.Var[S, Option[Folder]] with _CellViewImpl.Basic[S#Tx, Option[Folder]] {

    type Repr = Option[stm.Folder[S]]

    def repr(implicit tx: S#Tx): Option[stm.Folder[S]] =
      h().attr.$[stm.Folder](key)

    implicit def serializer: Serializer[S#Tx, S#Acc, Option[stm.Folder[S]]] =
      Serializer.option

    def repr_=(value: Option[stm.Folder[S]])(implicit tx: S#Tx): Unit = {
      val a = h().attr
      value match {
        case Some(f)  => a.put(key, f)
        case None     => a.remove(key)
      }
    }

    def lift(value: Option[Folder])(implicit tx: S#Tx): Option[stm.Folder[S]] =
      value.flatMap(_.peer)

    def apply()(implicit tx: S#Tx): Option[Folder] = repr.map(lower)

    private def lower(peer: stm.Folder[S])(implicit tx: S#Tx): Folder =
      new Impl(tx.newHandle(peer), tx.system)

    def update(v: Option[Folder])(implicit tx: S#Tx): Unit = {
      val peer = v.flatMap(_.peer)
      repr = peer
    }

    def react(fun: S#Tx => Option[Folder] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      new Observation[S](h().attr, key, fun, tx)
  }

  private final class Observation[S <: Sys[S]](attr: AttrMap[S], key: String, fun: S#Tx => Option[Folder] => Unit,
                                               tx0: S#Tx) extends Disposable[S#Tx] {
    private[this] val ref = Ref(Option.empty[(stm.Folder[S], Disposable[S#Tx])])

    private def mkFObs(f: stm.Folder[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      f.changed.react { implicit tx => _ =>
        val ex = lower(f)
        fun(tx)(Some(ex))
      }

    private def setObj(fOpt: Option[stm.Folder[S]])(implicit tx: S#Tx): Boolean =
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

    private def setObjAndFire(fOpt: Option[stm.Folder[S]])(implicit tx: S#Tx): Unit =
      if (setObj(fOpt)) fun(tx)(fOpt.map(lower))

    private def init()(implicit tx: S#Tx): Unit =
      setObj(attr.$[stm.Folder](key))

    init()(tx0)

    private def lower(peer: stm.Folder[S])(implicit tx: S#Tx): Folder =
      new Impl(tx.newHandle(peer), tx.system)

    private[this] val attrObs = attr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case stm.Obj.AttrAdded    (`key`    , f: stm.Folder[S]) => setObjAndFire(Some(f))
        case stm.Obj.AttrAdded    (`key`    , _)                => setObjAndFire(None)
        case stm.Obj.AttrRemoved  (`key`    , _: stm.Folder[S]) => setObjAndFire(None)
        case stm.Obj.AttrReplaced (`key`, _ , f: stm.Folder[S]) => setObjAndFire(Some(f))
        case stm.Obj.AttrReplaced (`key`, _ , _)                => setObjAndFire(None)
        case _ =>
      }
    } (tx0)

    def dispose()(implicit tx: S#Tx): Unit = {
      attrObs.dispose()
      ref.swap(None).foreach(_._2.dispose())
    }
  }

  implicit object Bridge extends Obj.Bridge[Folder] with Aux.Factory {
    final val id = 2001

    type Repr[S <: Sys[S]] = stm.Folder[S]

//    def mkObj[S <: Sys[S]](value: Folder)(implicit tx: S#Tx): stm.Folder[S] =
//      stm.Folder()

    def readIdentifiedAux(in: DataInput): Aux = this

    implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, stm.Folder[S]] =
      stm.Folder.serializer

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Folder]] =
      new CellViewImpl(tx.newHandle(obj), key)
  }

  private abstract class ExpandedImpl[S <: Sys[S], A](in: IExpr[S, Folder], init: A, tx0: S#Tx)
                                                     (implicit protected val targets: ITargets[S])
    extends IExpr[S, A] with IGenerator[S, Change[A]] with Caching {

    private[this] val obs   = Ref[Disposable[S#Tx]](Disposable.empty)
    private[this] val cache = Ref(init)

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): A

    private def setObj(v: Folder)(implicit tx: S#Tx): Option[Change[A]] = {
      obs.swap(Disposable.empty).dispose()
      // XXX TODO --- should we also fire when size has been non-zero and v.peer is empty?
      v.peer.flatMap { f =>
        val newObs = f.changed.react { implicit tx => upd =>
          val now     = mapValue(upd.list)
          val before  = cache.swap(now)
          if (before != now) fire(Change(before, now))
        }
        obs() = newObs
        val now     = mapValue(f)
        val before  = cache.swap(now)
        if (before != now) Some(Change(before, now)) else None
      }
    }

    in.changed.--->(this)(tx0)
    setObj(in.value(tx0))(tx0)

    def value(implicit tx: S#Tx): A = cache()

    def changed: IEvent[S, Change[A]] = this

    def dispose()(implicit tx: S#Tx): Unit = {
      in.changed.-/->(this)
      obs.swap(Disposable.empty).dispose()
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[A]] =
      if (pull.isOrigin(this)) Some(pull.resolve)
      else {
        pull(in.changed).flatMap { ch =>
          setObj(ch.now)
        }
      }
  }

  private final class SizeExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                               (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Int](in, 0, tx0) {

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Int = f.size
  }

  final case class Size(in: Ex[Folder]) extends Ex[Int] {
    override def productPrefix: String = s"Folder$$Size" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Int]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new SizeExpanded(in.expand[S], tx)
    }
  }

  private final class IsEmptyExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                                  (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Boolean](in, true, tx0) {

    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Boolean = f.isEmpty
  }

  final case class IsEmpty(in: Ex[Folder]) extends Ex[Boolean] {
    override def productPrefix: String = s"Folder$$IsEmpty" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Boolean]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new IsEmptyExpanded(in.expand[S], tx)
    }
  }

  private final class NonEmptyExpanded[S <: Sys[S]](in: IExpr[S, Folder], tx0: S#Tx)
                                                  (implicit targets: ITargets[S])
    extends ExpandedImpl[S, Boolean](in, false, tx0) {
    
    protected def mapValue(f: stm.List[S, stm.Obj[S]])(implicit tx: S#Tx): Boolean = f.nonEmpty
  }

  final case class NonEmpty(in: Ex[Folder]) extends Ex[Boolean] {
    override def productPrefix: String = s"Folder$$NonEmpty" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Boolean]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NonEmptyExpanded(in.expand[S], tx)
    }
  }

  private final class AppendExpanded[S <: Sys[S], A](in: IExpr[S, Folder], elem: IExpr[S, A])
                                                    (implicit bridge: Obj.Bridge[A])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      in.value.peer.foreach { f =>
        val v   = elem.value
        val obj = bridge.mkObj(v)
        f.addLast(obj)
      }
    }
  }

  private final class PrependExpanded[S <: Sys[S], A](in: IExpr[S, Folder], elem: IExpr[S, A])
                                                    (implicit bridge: Obj.Bridge[A])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      in.value.peer.foreach { f =>
        val v   = elem.value
        val obj = bridge.mkObj(v)
        f.addHead(obj)
      }
    }
  }

  final case class Append[A](in: Ex[Folder], elem: Ex[A])(implicit bridge: Obj.Bridge[A])
    extends Act with ProductWithAux {

    override def productPrefix: String = s"Folder$$Append" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new AppendExpanded(in.expand[S], elem.expand[S])

    def aux: List[Aux] = bridge :: Nil
  }

  final case class Prepend[A](in: Ex[Folder], elem: Ex[A])(implicit bridge: Obj.Bridge[A])
    extends Act with ProductWithAux {

    override def productPrefix: String = s"Folder$$Prepend" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new PrependExpanded(in.expand[S], elem.expand[S])

    def aux: List[Aux] = bridge :: Nil
  }

  implicit final class Ops(private val f: Ex[Folder]) extends AnyVal {
    def prepend[A](elem: Ex[A])(implicit bridge: Obj.Bridge[A]): Act = Prepend(f, elem)
    def append [A](elem: Ex[A])(implicit bridge: Obj.Bridge[A]): Act = Append (f, elem)
    def +=     [A](elem: Ex[A])(implicit bridge: Obj.Bridge[A]): Act = append(elem)

    def size    : Ex[Int    ] = Size    (f)
    def isEmpty : Ex[Boolean] = IsEmpty (f)
    def nonEmpty: Ex[Boolean] = NonEmpty(f)
  }
}
trait Folder extends Obj {
  type Peer[~ <: Sys[~]] = stm.Folder[~]
}