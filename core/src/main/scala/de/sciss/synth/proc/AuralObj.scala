/*
 *  AuralObj.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys, TxnLike}
import de.sciss.lucre.synth.{NodeRef, Sys => SSys}
import de.sciss.synth.proc.impl.{AuralActionImpl, AuralEnsembleImpl, AuralFolderImpl, AuralObjImpl => Impl, AuralProcImpl, AuralTimelineImpl}

import scala.language.higherKinds

object AuralObj {
  import de.sciss.synth.proc.{Action => _Action, Ensemble => _Ensemble, Folder => _Folder, Proc => _Proc, Timeline => _Timeline}

  trait Factory {
    def typeId: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](obj: Repr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = Impl(obj)

  /* The target state indicates the eventual state the process should have,
     independent of the current state which might not yet be ready.
   */
  sealed trait TargetState {
    def completed: AuralView.State
  }
  case object TargetStop extends TargetState {
    def completed: AuralView.State = AuralView.Stopped
  }
  case object TargetPrepared extends TargetState {
    def completed: AuralView.State = AuralView.Prepared
  }
  final case class TargetPlaying(wallClock: Long, timeRef: TimeRef) extends TargetState {
    def completed: AuralView.State = AuralView.Playing

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    override def toString = s"TargetPlaying(wallClock = $wallClock, timeRef = $timeRef)"
  }

  // -------------- sub-types --------------

  // ---- proc ----

  object Proc extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Proc[S]

    def typeId: Int = _Proc.typeId

    def apply[S <: SSys[S]](obj: _Proc[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] =
      AuralProcImpl(obj)

    sealed trait Update[S <: Sys[S]] {
      def proc: Proc[S]
      def key: String
    }
    sealed trait AttrUpdate[S <: Sys[S]] extends Update[S] {
      def attr: AuralAttribute[S]
      final def key: String = attr.key
    }
    sealed trait OutputUpdate[S <: Sys[S]] extends Update[S] {
      def output: AuralOutput[S]
      final def key: String = output.key
    }

    final case class AttrAdded  [S <: Sys[S]](proc: Proc[S], attr: AuralAttribute[S])
      extends AttrUpdate[S]

    final case class AttrRemoved[S <: Sys[S]](proc: Proc[S], attr: AuralAttribute[S])
      extends AttrUpdate[S]

    final case class OutputAdded  [S <: Sys[S]](proc: Proc[S], output: AuralOutput[S])
      extends OutputUpdate[S]

    final case class OutputRemoved[S <: Sys[S]](proc: Proc[S], output: AuralOutput[S])
      extends OutputUpdate[S]
  }
  trait Proc[S <: Sys[S]] extends AuralObj[S] {
    override def objH: stm.Source[S#Tx, _Proc[S]]

    /** The node reference associated with the process. A `Some` value indicates that
      * at least one instance view is playing, whereas a `None` value indicates that
      * there is no actively playing instance view at the moment.
      */
    def nodeOption(implicit tx: TxnLike): Option[NodeRef]

    def targetState(implicit tx: S#Tx): AuralView.State

    implicit def context: AuralContext[S]

    def ports: Observable[S#Tx, Proc.Update[S]]

    def getAttr  (key: String)(implicit tx: S#Tx): Option[AuralAttribute[S]]
    def getOutput(key: String)(implicit tx: S#Tx): Option[AuralOutput   [S]]
  }

  // ---- container ----

  object Container {
    sealed trait Update[S <: Sys[S], +Repr] {
      def container: Repr
    }
    final case class ViewAdded[S <: Sys[S], Repr](container: Repr, id: S#Id, view: AuralObj[S])
      extends Update[S, Repr]

    final case class ViewRemoved[S <: Sys[S], Repr](container: Repr, id: S#Id, view: AuralObj[S])
      extends Update[S, Repr]
  }
  trait Container[S <: Sys[S], +Repr <: Container[S, Repr]] extends AuralObj[S] {
    /** Monitors the _active_ views, i.e. views which are
      * intersecting with the current transport position.
      */
    def contents: Observable[S#Tx, Container.Update[S, Repr]]

    /** Returns the set of _active_ views, i.e. views which are intersecting
      * with the current transport position.
      */
    def views(implicit tx: S#Tx): Set[AuralObj[S]]

    def getViewById(id: S#Id)(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  // ---- timeline ----

  object Timeline extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Timeline[S]

    def typeId: Int = _Timeline.typeId

    def apply[S <: SSys[S]](obj: _Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] =
      AuralTimelineImpl(obj)

//    /** Creates an empty view that can be manually populated by calling `addObject`. */
//    def empty[S <: SSys[S]](obj: _Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): Manual[S] =
//      AuralTimelineImpl.empty(obj)

    trait Manual[S <: Sys[S]] extends Timeline[S] {
      // def addObject   (timed: _Timeline.Timed[S])(implicit tx: S#Tx): Unit
      // def removeObject(timed: _Timeline.Timed[S])(implicit tx: S#Tx): Unit
      def addObject   (id: S#Id, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit
      def removeObject(id: S#Id, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit
    }
  }
  trait Timeline[S <: Sys[S]] extends Container[S, Timeline[S]] {
    override def objH: stm.Source[S#Tx, _Timeline[S]]

    def getView(timed: _Timeline.Timed[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  // ---- ensemble ----

  trait FolderLike[S <: Sys[S], Repr <: FolderLike[S, Repr]] extends Container[S, Repr] {
    def folder(implicit tx: S#Tx): _Folder[S]

    def getView(obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  object Ensemble extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Ensemble[S]

    def typeId: Int = _Ensemble.typeId

    def apply[S <: SSys[S]](obj: _Ensemble[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] =
      AuralEnsembleImpl(obj)
  }
  trait Ensemble[S <: Sys[S]] extends FolderLike[S, Ensemble[S]] {
    override def objH: stm.Source[S#Tx, _Ensemble[S]]
  }

  // ---- folder ----

  object Folder extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Folder[S]

    def typeId: Int = _Folder.typeId

    def apply[S <: SSys[S]](obj: _Folder[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Folder[S] =
      AuralFolderImpl(obj)
  }
  trait Folder[S <: Sys[S]] extends FolderLike[S, Folder[S]] {
    override def objH: stm.Source[S#Tx, _Folder[S]]
  }

  // ---- action ----

  object Action extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Action[S]

    def typeId: Int = _Action.typeId

    def apply[S <: SSys[S]](obj: _Action[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] =
      AuralActionImpl(obj)
  }
  trait Action[S <: Sys[S]] extends AuralObj[S] {
    override def objH: stm.Source[S#Tx, _Action[S]]
  }
}
trait AuralObj[S <: Sys[S]] extends AuralView[S, Unit] {
  def play()(implicit tx: S#Tx): Unit = play(TimeRef.Undefined, ())
}