/*
 *  Runner.scala
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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, WorkspaceHandle}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.impl.{ActionRunnerImpl, RunnerHandlerImpl => Impl}
import de.sciss.synth.proc.{Action => _Action}

import scala.language.higherKinds

object Runner {
  sealed trait State
  case object Stopped   extends State
  case object Preparing extends State
  case object Prepared  extends State
  case object Running   extends State

  object Handler {
    sealed trait Update[S <: stm.Sys[S]]
    final case class Added  [S <: stm.Sys[S]](r: Runner[S]) extends Update[S]
    final case class Removed[S <: stm.Sys[S]](r: Runner[S]) extends Update[S]

    /** Finds an existing handler for the given workspace; returns this handler or
      * creates a new one if not found.
      */
    def apply[S <: Sys[S]]()(implicit tx: S#Tx, cursor: Cursor[S], workspace: WorkspaceHandle[S]): Handler[S] =
      Impl()

    /** Creates a new handler. */
    def apply[S <: Sys[S]](genContext: GenContext[S], scheduler: Scheduler[S], auralSystem: AuralSystem)
                          (implicit tx: S#Tx, cursor: Cursor[S], workspace: WorkspaceHandle[S]): Handler[S] =
      Impl(genContext, scheduler, auralSystem)
  }
  trait Handler[S <: stm.Sys[S]] extends Observable[S#Tx, Handler.Update[S]] with Disposable[S#Tx] {
    implicit def workspace  : WorkspaceHandle [S]
    implicit def cursor     : Cursor          [S]
    implicit def genContext : GenContext      [S]
    implicit def scheduler  : Scheduler       [S]

    def auralSystem: AuralSystem

    def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]]
  }
  
  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def getFactory(tpe: Obj.Type): Option[Factory] = Impl.getFactory(tpe)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, h: Handler[S]): Option[Runner[S]] =
    h.mkRunner(obj)

  trait Factory {
    def prefix      : String
    def humanName   : String
    def tpe         : Obj.Type

    /** Whether the factory maintains a singleton instance of a runner that will
      * be returned for multiple `mkRunner` calls (maintaining an internal use count
      * based on `mkRunner` and `dispose`) or not. Most objects will ''not'' have singleton runners.
      */
    def isSingleton : Boolean

    type Repr[~ <: Sys[~]] <: Obj[~]

    private[proc] def mkRunner[S <: Sys[S]](obj: Repr[S], h: Handler[S])(implicit tx: S#Tx): Runner[S]
  }

  // ---- factories ----


  // ---- Action ----

  object Action extends Factory {
    final val prefix          = "Action"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Action

    def isSingleton     : Boolean = false
//    def isInstantaneous : Boolean = true

    type Repr[~ <: Sys[~]] = _Action[~]

    private[proc] def mkRunner[S <: Sys[S]](obj: _Action[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
      ActionRunnerImpl(obj, h)
  }
}
trait Runner[S <: stm.Sys[S]] extends ViewBase[S, Unit] {
  def factory: Runner.Factory

  def messages(implicit tx: S#Tx): Any

  val handler: Runner.Handler[S]
/*
- allow both for a `self` and an `invoker` (`Action.Universe`)
- should we have an `value: Any` as in `Action.Universe`?
- actually, `invoker` and potential `value` should go into `play` and/or `prepare`
 */
}
