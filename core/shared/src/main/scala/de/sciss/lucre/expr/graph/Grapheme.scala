/*
 *  Grapheme.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, BinaryMappedObjIExpr, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase, UnaryMappedObjIExpr}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{CellView, Context, IAction}
import de.sciss.lucre.{Adjunct, Disposable, IExpr, ITargets, IdentMap, LongObj, ProductWithAdjuncts, Source, Sys, Txn, Obj => LObj}
import de.sciss.proc
import de.sciss.serial.{DataInput, TFormat}

object Grapheme extends ProductReader[Ex[Grapheme]] {
  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Grapheme] with Obj.Make = Apply()

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Grapheme] = {
    require (arity == 0 && adj == 0)
    Grapheme()
  }

  private[lucre] object Empty extends Grapheme {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] = None

    override def toString: String = "Grapheme<empty>"
  }

  private final class ApplyExpanded[T <: Txn[T]](implicit targets: ITargets[T])
    extends ExpandedObjMakeImpl[T, Grapheme] {

    protected def empty: Grapheme = Empty

    protected def make()(implicit tx: T): Grapheme = {
      val peer = proc.Grapheme[T]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Grapheme] with Act with Obj.Make {
    override def productPrefix: String = "Grapheme" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Grapheme] with IAction[T]

    def make: Act = this

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ApplyExpanded[T]
    }
  }

//  // used by Mellite (no transaction available)
//  private[lucre] def wrapH[T <: Txn[T]](peer: Source[T, proc.Grapheme[T]], system: Sys): Grapheme =
//    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: proc.Grapheme[T])(implicit tx: T): Grapheme =
    new Impl[T](tx.newHandle(peer), tx.system)

  private final class Impl[T <: Txn[T]](in: Source[T, proc.Grapheme[T]], system: Sys)
    extends ObjImplBase[T, proc.Grapheme](in, system) with Grapheme {

    override type Peer[~ <: Txn[~]] = proc.Grapheme[~]
  }

  private final class CellViewImpl[T <: Txn[T]](h: Source[T, LObj[T]], key: String)
    extends ObjCellViewVarImpl[T, proc.Grapheme, Grapheme](h, key) {

    implicit def format: TFormat[T, Option[proc.Grapheme[T]]] =
      TFormat.option

    protected def lower(peer: proc.Grapheme[T])(implicit tx: T): Grapheme =
      wrap(peer)
  }

  implicit object Bridge extends Obj.Bridge[Grapheme] with HasDefault[Grapheme] with Adjunct.Factory {
    final val id = 2005

    type Repr[T <: Txn[T]] = proc.Grapheme[T]

    def defaultValue: Grapheme = Empty

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Grapheme]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Grapheme]] =
      new AbstractCtxCellView[T, Grapheme](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[Grapheme] = value match {
          case gr: Grapheme => Some(gr)
          case _            => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[Grapheme] = obj match {
          case peer: proc.Grapheme[T] => Some(wrap(peer))
          case _                      => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Grapheme] =
      obj.attr.$[proc.Grapheme](key).map(wrap(_))

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Grapheme] = obj match {
      case a: proc.Grapheme[T]  => Some(wrap(a))
      case _                    => None
    }
  }

  // ---- actions ----

  private final class AddExpanded[T <: Txn[T], A](in: IExpr[T, Grapheme], time: IExpr[T, Long],
                                                  elem: IExpr[T, A])
                                                 (implicit source: Obj.Source[A])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      for {
        gr  <- in.value.peer[T]
        grm <- gr.modifiableOption
      } {
        val timeV   = time.value
        val elemV   = elem.value
        val timeObj = LongObj.newVar[T](timeV)
        val elemObj = source.toObj[T](elemV)
        // XXX TODO: undoable: EditGrapheme.add(grm, timeObj, elemObj)
        grm.add(timeObj, elemObj)
      }
  }

  object Add extends ProductReader[Add[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Add[_] = {
      require (arity == 3 && adj == 1)
      val _in   = in.readEx[Grapheme]()
      val _time = in.readEx[Long]()
      val _elem = in.readEx[Any]()
      val _source: Obj.Source[Any] = in.readAdjunct()
      new Add[Any](_in, _time, _elem)(_source)
    }
  }
  final case class Add[A](in: Ex[Grapheme], time: Ex[Long], elem: Ex[A])(implicit source: Obj.Source[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Grapheme$$Add" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new AddExpanded(in.expand[T], time.expand[T], elem.expand[T])

    override def adjuncts: List[Adjunct] = source :: Nil
  }

  private final class AddAllExpanded[T <: Txn[T], A](in: IExpr[T, Grapheme], pairs: IExpr[T, Seq[(Long, A)]])
                                                    (implicit source: Obj.Source[A])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      for {
        gr  <- in.value.peer[T]
        grm <- gr.modifiableOption
      } {
        val pairsV  = pairs.value
        pairsV.foreach { case (timeV, elemV) =>
          val timeObj = LongObj.newVar[T](timeV)
          val elemObj = source.toObj[T](elemV)
          // XXX TODO: undoable: EditGrapheme.add(grm, timeObj, elemObj)
          grm.add(timeObj, elemObj)
        }
      }
  }

  object AddAll extends ProductReader[AddAll[_]] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): AddAll[_] = {
      require (arity == 2 && adj == 1)
      val _in     = in.readEx[Grapheme]()
      val _pairs  = in.readEx[Seq[(Long, Any)]]()
      val _source: Obj.Source[Any] = in.readAdjunct()
      new AddAll[Any](_in, _pairs)(_source)
    }
  }
  final case class AddAll[A](in: Ex[Grapheme], pairs: Ex[Seq[(Long, A)]])(implicit source: Obj.Source[A])
    extends Act with ProductWithAdjuncts {

    override def productPrefix: String = s"Grapheme$$AddAll" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new AddAllExpanded(in.expand[T], pairs.expand[T])

    override def adjuncts: List[Adjunct] = source :: Nil
  }

  private final class RemoveExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], time: IExpr[T, Long],
                                                  elem: IExpr[T, Obj])
    extends IActionImpl[T] {

    private def findTime(gr: proc.Grapheme[T], elemObj: LObj[T])(implicit tx: T): Option[LongObj[T]] = {
      val timeV = time.value
      gr.intersect(timeV).collectFirst {
        case e if e.key.value == timeV && e.value == elemObj => e.key
      }
    }

    def executeAction()(implicit tx: T): Unit =
      for {
        gr      <- in.value.peer[T]
        grm     <- gr.modifiableOption
        elemObj <- elem.value.peer[T]
        timeObj <- findTime(gr, elemObj)
      } {
        // XXX TODO: undoable: EditGrapheme.unlinkAndRemove(grm, timeObj, elemObj)
        grm.remove(timeObj, elemObj)
      }
  }

  object Remove extends ProductReader[Remove] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Remove = {
      require (arity == 3 && adj == 0)
      val _in     = in.readEx[Grapheme]()
      val _time   = in.readEx[Long]()
      val _elem   = in.readEx[Obj]()
      new Remove(_in, _time, _elem)
    }
  }
  final case class Remove(in: Ex[Grapheme], time: Ex[Long], elem: Ex[Obj])
    extends Act {

    override def productPrefix: String = s"Grapheme$$Remove" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new RemoveExpanded(in.expand[T], time.expand[T], elem.expand[T])
  }

  private final class RemoveAtExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], time: IExpr[T, Long])
    extends IActionImpl[T] {

    private def findTime(gr: proc.Grapheme[T])(implicit tx: T): proc.Grapheme.Leaf[T] = {
      val timeV = time.value
      val leaf  = gr.intersect(timeV)
      if (leaf.exists(_.key.value == timeV)) leaf else Vector.empty
    }

    def executeAction()(implicit tx: T): Unit =
      for {
        gr      <- in.value.peer[T]
        grm     <- gr.modifiableOption
        entry   <- findTime(gr)
      } {
        // XXX TODO: undoable: EditGrapheme.unlinkAndRemoveAt(grm, timeObj, elemObj)
        grm.remove(entry.key, entry.value)
      }
  }

  object RemoveAt extends ProductReader[RemoveAt] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RemoveAt = {
      require (arity == 2 && adj == 0)
      val _in     = in.readEx[Grapheme]()
      val _time   = in.readEx[Long]()
      new RemoveAt(_in, _time)
    }
  }
  final case class RemoveAt(in: Ex[Grapheme], time: Ex[Long])
    extends Act {

    override def productPrefix: String = s"Grapheme$$RemoveAt" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new RemoveAtExpanded(in.expand[T], time.expand[T])
  }

  private final class ClearExpanded[T <: Txn[T]](in: IExpr[T, Grapheme])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      for {
        gr      <- in.value.peer[T]
        grm     <- gr.modifiableOption
      } {
        // XXX TODO: undoable
        grm.clear()
      }
  }

  object Clear extends ProductReader[Clear] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Clear = {
      require (arity == 1 && adj == 0)
      val _in     = in.readEx[Grapheme]()
      new Clear(_in)
    }
  }
  final case class Clear(in: Ex[Grapheme])
    extends Act {

    override def productPrefix: String = s"Grapheme$$Clear" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ClearExpanded(in.expand[T])
  }

  // ---- expressions ----

  private final class FirstEventExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], tx0: T)(implicit targets: ITargets[T])
    extends UnaryMappedGraphemeIExpr[T, Option[Long]](in, tx0) {

    override protected def mapValue(inOpt: Option[proc.Grapheme[T]], isInit: Boolean)(implicit tx: T): Option[Long] =
      inOpt.flatMap(_.firstEvent)
  }

  object FirstEvent extends ProductReader[FirstEvent] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): FirstEvent = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Grapheme]()
      new FirstEvent(_in)
    }
  }
  final case class FirstEvent(in: Ex[Grapheme]) extends Ex[Option[Long]] {
    override def productPrefix: String = s"Grapheme$$FirstEvent" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[Long]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new FirstEventExpanded(in.expand[T], tx)
    }
  }

  private final class LastEventExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], tx0: T)(implicit targets: ITargets[T])
    extends UnaryMappedGraphemeIExpr[T, Option[Long]](in, tx0) {

    override protected def mapValue(inOpt: Option[proc.Grapheme[T]], isInit: Boolean)(implicit tx: T): Option[Long] =
      inOpt.flatMap(_.lastEvent)
  }

  object LastEvent extends ProductReader[LastEvent] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): LastEvent = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Grapheme]()
      new LastEvent(_in)
    }
  }
  final case class LastEvent(in: Ex[Grapheme]) extends Ex[Option[Long]] {
    override def productPrefix: String = s"Grapheme$$LastEvent" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[Long]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new LastEventExpanded(in.expand[T], tx)
    }
  }

  private abstract class UnaryMappedGraphemeIExpr[T <: Txn[T], A](in: IExpr[T, Grapheme], tx0: T)
                                                                 (implicit targets: ITargets[T])
    extends UnaryMappedObjIExpr[T, proc.Grapheme, Grapheme, A](in, tx0) {

    override protected def observeObj(gr: proc.Grapheme[T])(implicit tx: T): Disposable[T] =
      gr.changed.react { implicit tx => upd =>
        val now = mapValue(Some(upd.pin), isInit = false)
        updateFromObj(now)
      }
  }

  private abstract class BinaryMappedGraphemeIExpr[T <: Txn[T], C, A](in: IExpr[T, Grapheme], b: IExpr[T, C], tx0: T)
                                                                  (implicit targets: ITargets[T])
    extends BinaryMappedObjIExpr[T, proc.Grapheme, Grapheme, C, A](in, b, tx0) {

    override protected def observeObj(gr: proc.Grapheme[T])(implicit tx: T): Disposable[T] =
      gr.changed.react { implicit tx => upd =>
        // it's probably faster and less error-prone to just run `mapValue` and see if that
        // has a relevant change, that analyzing the updates beforehand.
        val now = mapValue(Some(upd.pin), b.value, isInit = false)
        updateFromObj(now)
      }
  }

  private final class EventBeforeExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], time: IExpr[T, Long], tx0: T)
                                                      (implicit targets: ITargets[T])
    extends BinaryMappedGraphemeIExpr[T, Long, Option[Long]](in, time, tx0) {

    override protected def mapValue(/*before: Option[Long],*/ inOpt: Option[proc.Grapheme[T]], b: Long,
                                    isInit: Boolean)
                                   (implicit tx: T): Option[Long] =
      inOpt.flatMap(_.eventBefore(b))

//    protected def mapValue(inV: Grapheme, timeV: Long)(implicit tx: T): Option[Long] =
//      inV.peer[T].flatMap(_.eventBefore(timeV))
  }

  object EventBefore extends ProductReader[EventBefore] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): EventBefore = {
      require (arity == 2 && adj == 0)
      val _in   = in.readEx[Grapheme]()
      val _time = in.readEx[Long    ]()
      new EventBefore(_in, _time)
    }
  }
  final case class EventBefore(in: Ex[Grapheme], time: Ex[Long]) extends Ex[Option[Long]] {
    override def productPrefix: String = s"Grapheme$$EventBefore" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[Long]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new EventBeforeExpanded(in.expand[T], time.expand[T], tx)
    }
  }

  private final class EventAfterExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], time: IExpr[T, Long], tx0: T)
                                                     (implicit targets: ITargets[T])
    extends BinaryMappedGraphemeIExpr[T, Long, Option[Long]](in, time, tx0) {

    override protected def mapValue(inOpt: Option[proc.Grapheme[T]], b: Long, isInit: Boolean)
                                   (implicit tx: T): Option[Long] =
      inOpt.flatMap(_.eventAfter(b))

//    protected def mapValue(inV: Grapheme, timeV: Long)(implicit tx: T): Option[Long] =
//      inV.peer[T].flatMap(_.eventAfter(timeV))
  }

  object EventAfter extends ProductReader[EventAfter] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): EventAfter = {
      require (arity == 2 && adj == 0)
      val _in   = in.readEx[Grapheme]()
      val _time = in.readEx[Long    ]()
      new EventAfter(_in, _time)
    }
  }
  final case class EventAfter(in: Ex[Grapheme], time: Ex[Long]) extends Ex[Option[Long]] {
    override def productPrefix: String = s"Grapheme$$EventAfter" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[Long]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new EventAfterExpanded(in.expand[T], time.expand[T], tx)
    }
  }

  private final class ValueAtExpanded[T <: Txn[T]](in: IExpr[T, Grapheme], time: IExpr[T, Long],
                                                   idMap: IdentMap[T, Obj], tx0: T)
                                                     (implicit targets: ITargets[T])
    extends BinaryMappedGraphemeIExpr[T, Long, Option[Obj]](in, time, tx0) {

    override def dispose()(implicit tx: T): Unit = {
      idMap.dispose()
      super.dispose()
    }

    override protected def mapValue(inOpt: Option[proc.Grapheme[T]], b: Long, isInit: Boolean)
                                   (implicit tx: T): Option[Obj] = {
      // big action to "cache" Obj instances, so we do not fire unnecessarily
      // when there are no actual changes (`Obj.equals` cannot capture identical peers).
      inOpt.flatMap { in =>
        val valueOpt = in.valueAt(b)
        valueOpt.map { valueNow =>
          idMap.getOrElse(valueNow.id, {
            val wrapNow = Obj.wrap[T](valueNow)
            // `mapValue` is called in constructor, so be careful to check if `ref` was already initialized
            if (!isInit) ref().foreach { wrapBefore =>
              wrapBefore.peer[T].foreach { valueBefore =>
                idMap.remove(valueBefore.id)
              }
            }
            idMap.put(valueNow.id, wrapNow)
            wrapNow
          })
        }
      }
    }

    //    protected def mapValue(inV: Grapheme, timeV: Long)(implicit tx: T): Option[Obj] =
//      inV.peer[T].flatMap(_.valueAt(timeV).map(Obj.wrap[T](_)))
  }

  object ValueAt extends ProductReader[ValueAt] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): ValueAt = {
      require (arity == 2 && adj == 0)
      val _in   = in.readEx[Grapheme]()
      val _time = in.readEx[Long    ]()
      new ValueAt(_in, _time)
    }
  }
  final case class ValueAt(in: Ex[Grapheme], time: Ex[Long]) extends Ex[Option[Obj]] {
    override def productPrefix: String = s"Grapheme$$ValueAt" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[Obj]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ValueAtExpanded(in.expand[T], time.expand[T], tx.newIdentMap[Obj], tx)
    }
  }

  implicit final class Ops(private val gr: Ex[Grapheme]) extends AnyVal {
    def add   [A](time: Ex[Long], elem: Ex[A  ])(implicit source: Obj.Source[A]): Act   = Add     (gr, time, elem)
    def remove   (time: Ex[Long], elem: Ex[Obj])                                : Act   = Remove  (gr, time, elem)

    /** Removes all elements at a given time (if any exist) */
    def removeAt (time: Ex[Long])                                               : Act   = RemoveAt(gr, time)

    // def size    : Ex[Int    ]   = ...

//    def isEmpty : Ex[Boolean]   = ...
//    def nonEmpty: Ex[Boolean]   = ...

    def clear: Act = Clear(gr)

    def addAll[A](pairs: Ex[Seq[(Long, A)]])(implicit source: Obj.Source[A]): Act = AddAll(gr, pairs)

//    /** Gets an entry at an exact point in time. */
//    def get(time: Ex[Long]): Ex[Option[Obj]]
//
//    /** Gets an element at an exact point in time. */
//    def getValue(time: Ex[Long]): Ex[Option[Obj]] = ...

//    /** Queries the entry valid for the given point in time. Unlike `get`, if there are no objects
//      * at the exact point in time, returns the last object before that point in time.*/
//    def at (time: Ex[Long]): Ex[Option[Obj]]

    /** Queries the element valid for the given point in time. Unlike `get`, if there are no objects
      * at the exact point in time, returns the last object before that point in time.*/
    def valueAt(time: Ex[Long]): Ex[Option[Obj]] = ValueAt(gr, time)

    /** If the grapheme is non-empty, the time of the first event. */
    def firstEvent: Ex[Option[Long]] = FirstEvent(gr)
    /** If the grapheme is non-empty, the time of the last event. */
    def lastEvent : Ex[Option[Long]] = LastEvent (gr)

    def eventBefore (time: Ex[Long]): Ex[Option[Long]] = EventBefore(gr, time)
    def eventAfter  (time: Ex[Long]): Ex[Option[Long]] = EventAfter (gr, time)

    // def children: Ex[Seq[Obj]] = ...
  }
}
trait Grapheme extends Obj {
  type Peer[~ <: Txn[~]] = proc.Grapheme[~]
}