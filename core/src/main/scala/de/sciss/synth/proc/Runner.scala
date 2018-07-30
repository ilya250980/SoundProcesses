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
import de.sciss.synth.proc.impl.{ActionRunnerImpl, ProcRunnerImpl, RunnerHandlerImpl => Impl}
import de.sciss.synth.proc.{Action => _Action, Proc => _Proc}

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

    def mkTransport()(implicit tx: S#Tx): Transport[S]

    def auralSystem: AuralSystem

    def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]]

    def runners(implicit tx: S#Tx): Iterator[Runner[S]]

    private[proc] def removeRunner(r: Runner[S])(implicit tx: S#Tx): Unit
  }
  
  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def getFactory(tpe: Obj.Type): Option[Factory] = Impl.getFactory(tpe)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, h: Handler[S]): Option[Runner[S]] =
    h.mkRunner(obj)

//  def orDummy[S <: stm.Sys[S]](obj: Obj[S])

//  def dummy[S <: stm.Sys[S]](implicit tx: S#Tx, h: Handler[S])

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

  // -------------------
  // ---- factories ----
  // -------------------

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

  // ---- Proc ----

  object Proc extends Factory {
    final val prefix          = "Proc"
    final val humanName       = "Process"
    def tpe       : Obj.Type  = _Proc

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Proc[~]

    private[proc] def mkRunner[S <: Sys[S]](obj: _Proc[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
      ProcRunnerImpl(obj, h)
  }
}
trait Runner[S <: stm.Sys[S]] extends ViewBase[S, Unit] {
  // def factory: Runner.Factory

  def messages(implicit tx: S#Tx): Any

  val handler: Runner.Handler[S]

  protected def disposeData()(implicit tx: S#Tx): Unit

  // this is implemented so there is no chance of forgetting
  // to remove the runner from the handler
  final def dispose()(implicit tx: S#Tx): Unit = {
    handler.removeRunner(this)
    disposeData()
  }

/*
- allow both for a `self` and an `invoker` (`Action.Universe`)
- should we have an `value: Any` as in `Action.Universe`?
- actually, `invoker` and potential `value` should go into `play` and/or `prepare`
 */
}
