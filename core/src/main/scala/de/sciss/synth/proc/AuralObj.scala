/*
 *  AuralObj.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys, TxnLike}
import de.sciss.lucre.synth.{NodeRef, Sys => SSys}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{AuralActionImpl, AuralEnsembleImpl, AuralFolderImpl, AuralProcImpl, AuralTimelineImpl, AuralObjImpl => Impl}

import scala.language.higherKinds

object AuralObj {
  import proc.{Action => _Action, Ensemble => _Ensemble, Proc => _Proc, Timeline => _Timeline}
  import stm.{Folder => _Folder}

  trait Factory {
    def tpe: Obj.Type

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](obj: Repr[S], attr: Runner.Attr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](obj: Obj[S], attr: Runner.Attr[S] = Runner.emptyAttr[S])
                         (implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = Impl(obj, attr = attr)

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
    type Repr[S <: Sys[S]] = _Proc[S]

    def tpe: Obj.Type = _Proc

    def apply[S <: SSys[S]](obj: _Proc[S], attr: Runner.Attr[S] = Runner.emptyAttr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] =
      AuralProcImpl(obj, attr)

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
    type Repr = _Proc[S]

    /** The node reference associated with the process. A `Some` value indicates that
      * at least one instance view is playing, whereas a `None` value indicates that
      * there is no actively playing instance view at the moment.
      */
    def nodeOption(implicit tx: TxnLike): Option[NodeRef]

    def targetState(implicit tx: S#Tx): Runner.State

    implicit def context: AuralContext[S]

    def ports: Observable[S#Tx, Proc.Update[S]]

    // used by Nuages
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
  trait Container[S <: Sys[S], +Self <: Container[S, Self]] extends AuralObj[S] {
    /** Monitors the _active_ views, i.e. views which are
      * intersecting with the current transport position.
      */
    def contents: Observable[S#Tx, Container.Update[S, Self]]

    /** Returns the set of _active_ views, i.e. views which are intersecting
      * with the current transport position.
      */
    def views(implicit tx: S#Tx): Set[AuralObj[S]]

    def getViewById(id: S#Id)(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  // ---- timeline ----

  object Timeline extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Timeline[S]

    def tpe: Obj.Type = _Timeline

    def apply[S <: SSys[S]](obj: _Timeline[S], attr: Runner.Attr[S] = Runner.emptyAttr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] =
      AuralTimelineImpl(obj, attr)

    trait Manual[S <: Sys[S]] extends Timeline[S] {
      def addObject   (id: S#Id, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit
      def removeObject(id: S#Id, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit
    }
  }
  trait Timeline[S <: Sys[S]] extends Container[S, Timeline[S]] {
    type Repr = _Timeline[S]

    def getView(timed: _Timeline.Timed[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  // ---- ensemble ----

  trait FolderLike[S <: Sys[S], Self <: FolderLike[S, Self]] extends Container[S, Self] {
    def folder(implicit tx: S#Tx): _Folder[S]

    def getView(obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  }

  object Ensemble extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Ensemble[S]

    def tpe: Obj.Type = _Ensemble

    def apply[S <: SSys[S]](obj: _Ensemble[S], attr: Runner.Attr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] =
      AuralEnsembleImpl(obj, attr)
  }
  trait Ensemble[S <: Sys[S]] extends FolderLike[S, Ensemble[S]] {
    type Repr = _Ensemble[S]
  }

  // ---- folder ----

  object Folder extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Folder[S]

    def tpe: Obj.Type = _Folder

    def apply[S <: SSys[S]](obj: _Folder[S], attr: Runner.Attr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Folder[S] =
      AuralFolderImpl(obj, attr)
  }
  trait Folder[S <: Sys[S]] extends FolderLike[S, Folder[S]]

  // ---- action ----

  object Action extends AuralObj.Factory {
    type Repr[S <: Sys[S]] = _Action[S]

    def tpe: Obj.Type = _Action

    def apply[S <: SSys[S]](obj: _Action[S], attr: Runner.Attr[S])
                           (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] =
      AuralActionImpl(obj, attr)
  }
  trait Action[S <: Sys[S]] extends AuralObj[S] {
    type Repr = _Action[S]
  }
}
trait AuralObj[S <: Sys[S]] extends ObjViewBase[S, Unit] {
  def play()(implicit tx: S#Tx): Unit = run(TimeRef.Undefined, ())
}