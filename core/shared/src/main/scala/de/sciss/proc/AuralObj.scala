/*
 *  AuralObj.scala
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

package de.sciss.proc

import de.sciss.lucre
import de.sciss.lucre.synth.NodeRef
import de.sciss.lucre.{Ident, Obj, Observable, SpanLikeObj, Txn, TxnLike, synth}
import de.sciss.proc
import de.sciss.proc.impl.{AuralActionImpl, AuralControlImpl, AuralFolderImpl, AuralProcImpl, AuralTimelineImpl, AuralObjImpl => Impl}

object AuralObj {
  import lucre.{Folder => _Folder}
  import proc.{Action => _Action, Control => _Control, Proc => _Proc, Timeline => _Timeline}

  trait Factory {
    def tpe: Obj.Type

    type Repr[~ <: Txn[~]] <: Obj[~]

    def apply[T <: synth.Txn[T]](obj: Repr[T], attr: Runner.Attr[T])
                           (implicit tx: T, context: AuralContext[T]): AuralObj[T]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[T <: synth.Txn[T]](obj: Obj[T], attr: Runner.Attr[T] = Runner.emptyAttr[T])
                         (implicit tx: T, context: AuralContext[T]): AuralObj[T] = Impl(obj, attr = attr)

  /* The target state indicates the eventual state the process should have,
     independent of the current state which might not yet be ready.
   */
  sealed trait TargetState {
    def completed: Runner.State
  }
  case object TargetStop extends TargetState {
    def completed: Runner.State = Runner.Stopped
  }
  case object TargetPrepared extends TargetState {
    def completed: Runner.State = Runner.Prepared
  }
  final case class TargetPlaying(wallClock: Long, timeRef: TimeRef) extends TargetState {
    def completed: Runner.State = Runner.Running

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    override def toString = s"TargetPlaying(wallClock = $wallClock, timeRef = $timeRef)"
  }

  // -------------- sub-types --------------

  // ---- proc ----

  object Proc extends AuralObj.Factory {
    type Repr[T <: Txn[T]] = _Proc[T]

    def tpe: Obj.Type = _Proc

    def apply[T <: synth.Txn[T]](obj: _Proc[T], attr: Runner.Attr[T] = Runner.emptyAttr[T])
                           (implicit tx: T, context: AuralContext[T]): AuralObj.Proc[T] =
      AuralProcImpl(obj, attr)

    sealed trait Update[T <: Txn[T]] {
      def proc: Proc[T]
      def key: String
    }
    sealed trait AttrUpdate[T <: Txn[T]] extends Update[T] {
      def attr: AuralAttribute[T]
      final def key: String = attr.key
    }
    sealed trait OutputUpdate[T <: Txn[T]] extends Update[T] {
      def output: AuralOutput[T]
      final def key: String = output.key
    }

    final case class AttrAdded  [T <: Txn[T]](proc: Proc[T], attr: AuralAttribute[T])
      extends AttrUpdate[T]

    final case class AttrRemoved[T <: Txn[T]](proc: Proc[T], attr: AuralAttribute[T])
      extends AttrUpdate[T]

    final case class OutputAdded  [T <: Txn[T]](proc: Proc[T], output: AuralOutput[T])
      extends OutputUpdate[T]

    final case class OutputRemoved[T <: Txn[T]](proc: Proc[T], output: AuralOutput[T])
      extends OutputUpdate[T]
  }
  trait Proc[T <: Txn[T]] extends AuralObj[T] {
    type Repr = _Proc[T]

    /** The node reference associated with the process. A `Some` value indicates that
      * at least one instance view is playing, whereas a `None` value indicates that
      * there is no actively playing instance view at the moment.
      */
    def nodeOption(implicit tx: TxnLike): Option[NodeRef]

    def targetState(implicit tx: T): Runner.State

    implicit def context: AuralContext[T]

    def ports: Observable[T, Proc.Update[T]]

    // used by Nuages
    def getAttr  (key: String)(implicit tx: T): Option[AuralAttribute[T]]
    def getOutput(key: String)(implicit tx: T): Option[AuralOutput   [T]]
  }

  // ---- container ----

  object Container {
    sealed trait Update[T <: Txn[T], +Repr] {
      def container: Repr
    }
    final case class ViewAdded[T <: Txn[T], Repr](container: Repr, id: Ident[T], view: AuralObj[T])
      extends Update[T, Repr]

    final case class ViewRemoved[T <: Txn[T], Repr](container: Repr, id: Ident[T], view: AuralObj[T])
      extends Update[T, Repr]
  }
  trait Container[T <: Txn[T], +Self <: Container[T, Self]] extends AuralObj[T] {
    /** Monitors the _active_ views, i.e. views which are
      * intersecting with the current transport position.
      */
    def contents: Observable[T, Container.Update[T, Self]]

    /** Returns the set of _active_ views, i.e. views which are intersecting
      * with the current transport position.
      */
    def views(implicit tx: T): Set[AuralObj[T]]

    def getViewById(id: Ident[T])(implicit tx: T): Option[AuralObj[T]]
  }

  // ---- timeline ----

  object Timeline extends AuralObj.Factory {
    type Repr[T <: Txn[T]] = _Timeline[T]

    def tpe: Obj.Type = _Timeline

    def apply[T <: synth.Txn[T]](obj: _Timeline[T], attr: Runner.Attr[T] = Runner.emptyAttr[T])
                           (implicit tx: T, context: AuralContext[T]): AuralObj.Timeline[T] =
      AuralTimelineImpl(obj, attr)

    trait Manual[T <: Txn[T]] extends Timeline[T] {
      def addObject   (id: Ident[T], span: SpanLikeObj[T], obj: Obj[T])(implicit tx: T): Unit
      def removeObject(id: Ident[T], span: SpanLikeObj[T], obj: Obj[T])(implicit tx: T): Unit
    }
  }
  trait Timeline[T <: Txn[T]] extends Container[T, Timeline[T]] {
    type Repr = _Timeline[T]

    def getView(timed: _Timeline.Timed[T])(implicit tx: T): Option[AuralObj[T]]
  }

  // ---- ensemble ----

  trait FolderLike[T <: Txn[T], Self <: FolderLike[T, Self]] extends Container[T, Self] {
    def folder(implicit tx: T): _Folder[T]

    def getView(obj: Obj[T])(implicit tx: T): Option[AuralObj[T]]
  }

//  @deprecated("Should only use Folder now with Control/Action", since = "3.35.3")
//  object Ensemble extends AuralObj.Factory {
//    type Repr[T <: Txn[T]] = _Ensemble[T]
//
//    def tpe: Obj.Type = _Ensemble
//
//    def apply[T <: synth.Txn[T]](obj: _Ensemble[T], attr: Runner.Attr[T])
//                           (implicit tx: T, context: AuralContext[T]): AuralObj.Ensemble[T] =
//      AuralEnsembleImpl(obj, attr)
//  }
//  @deprecated("Should only use Folder now with Control/Action", since = "3.35.3")
//  trait Ensemble[T <: Txn[T]] extends FolderLike[T, Ensemble[T]] {
//    type Repr = _Ensemble[T]
//  }

  // ---- folder ----

  object Folder extends AuralObj.Factory {
    type Repr[T <: Txn[T]] = _Folder[T]

    def tpe: Obj.Type = _Folder

    def apply[T <: synth.Txn[T]](obj: _Folder[T], attr: Runner.Attr[T])
                           (implicit tx: T, context: AuralContext[T]): AuralObj.Folder[T] =
      AuralFolderImpl(obj, attr)
  }
  trait Folder[T <: Txn[T]] extends FolderLike[T, Folder[T]]

  // ---- action ----

  object Action extends AuralObj.Factory {
    type Repr[T <: Txn[T]] = _Action[T]

    def tpe: Obj.Type = _Action

    def apply[T <: synth.Txn[T]](obj: _Action[T], attr: Runner.Attr[T])
                           (implicit tx: T, context: AuralContext[T]): AuralObj.Action[T] =
      AuralActionImpl(obj, attr)
  }
  trait Action[T <: Txn[T]] extends AuralObj[T] {
    type Repr = _Action[T]
  }

  // ---- action ----

  object Control extends AuralObj.Factory {
    type Repr[T <: Txn[T]] = _Control[T]

    def tpe: Obj.Type = _Control

    def apply[T <: synth.Txn[T]](obj: _Control[T], attr: Runner.Attr[T])
                           (implicit tx: T, context: AuralContext[T]): AuralObj.Control[T] =
      AuralControlImpl(obj, attr)
  }
  trait Control[T <: Txn[T]] extends AuralObj[T] {
    type Repr = _Control[T]
  }
}
trait AuralObj[T <: Txn[T]] extends ObjViewBase[T, Unit] {
  def play()(implicit tx: T): Unit = run(TimeRef.Undefined, ())
}