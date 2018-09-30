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
import de.sciss.synth.proc.impl.{ActionRunnerImpl, ProcRunnerImpl, TimelineRunnerImpl, RunnerHandlerImpl => Impl}
import de.sciss.synth.proc.{Action => _Action, Proc => _Proc, Timeline => _Timeline}

import scala.language.higherKinds

object Runner {
  sealed trait State { def id: Int }
  case object Stopped   extends State { final val id = 0 }
  case object Preparing extends State { final val id = 1 }
  case object Prepared  extends State { final val id = 2 }
  case object Running   extends State { final val id = 3 }

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
  trait Handler[S <: stm.Sys[S]] extends Universe[S] with Observable[S#Tx, Handler.Update[S]] with Disposable[S#Tx] {
    private[proc] def removeRunner(r: Runner[S])(implicit tx: S#Tx): Unit
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

    def mkRunner[S <: Sys[S]](obj: Repr[S], h: Handler[S])(implicit tx: S#Tx): Runner[S]
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

    def mkRunner[S <: Sys[S]](obj: _Action[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
      ActionRunnerImpl(obj, h)
  }

  // ---- Proc ----

  object Proc extends Factory {
    final val prefix          = "Proc"
    final val humanName       = "Process"
    def tpe       : Obj.Type  = _Proc

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Proc[~]

    def mkRunner[S <: Sys[S]](obj: _Proc[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
      ProcRunnerImpl(obj, h)
  }

  object Timeline extends Factory {
    final val prefix          = "Timeline"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Timeline

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Timeline[~]

    def mkRunner[S <: Sys[S]](obj: _Timeline[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
      TimelineRunnerImpl(obj, h)
  }

  object Message {
    sealed trait Level
    case object Error   extends Level
    case object Warning extends Level
    case object Info    extends Level
  }
  final case class Message(time: Long, level: Message.Level, text: String)

  trait Messages[Tx] extends Observable[Tx, List[Message]] {
    def current(implicit tx: Tx): List[Message]
  }

  trait Progress[Tx] extends Observable[Tx, Double] {
    /** Zero to one. Note: negative numbers indicate indeterminate progress */
    def current(implicit tx: Tx): Double
  }
}
trait Runner[S <: stm.Sys[S]] extends ViewBase[S, Unit] {
  // def factory: Runner.Factory

  def messages: Runner.Messages[S#Tx] // (implicit tx: S#Tx): Any

  def progress: Runner.Progress[S#Tx]

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
