/*
 *  Runner.scala
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

package de.sciss.synth.proc

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.stm.{Disposable, Obj, Sys, Folder => _Folder}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.impl.{ActionRawRunnerImpl, ActionRunnerImpl, BasicAuralRunnerImpl, ControlRunnerImpl, FolderRunnerImpl, TimelineRunnerImpl, RunnerUniverseImpl => Impl}
import de.sciss.synth.proc.{Action => _Action, ActionRaw => _ActionRaw, Control => _Control, Proc => _Proc, Timeline => _Timeline}

import scala.util.Try

object Runner {
  sealed trait State {
    def id: Int

    /** Stopped, failed or done. */
    def idle: Boolean

    def done          : Boolean
    def failed        : Boolean
    def idleOrPrepared: Boolean
    def stoppedOrDone : Boolean
  }

  /** The initial state of an object and after `stop` has been called and performed.
    * If an object comes to a halt by itself, it will depend on the type of object
    * whether it goes back to `Stopped` or finishes with `Done`.
    */
  case object Stopped extends State {
    final val id              = 0
    final val idle            = true
    final val done            = false
    final val failed          = false
    final val idleOrPrepared  = true
    final val stoppedOrDone   = true
  }
  case object Preparing extends State {
    final val id              = 1
    final val idle            = false
    final val done            = false
    final val failed          = false
    final val idleOrPrepared  = false
    final val stoppedOrDone   = false
  }
  case object Prepared extends State {
    final val id              = 2
    /** Note: this reports `false` */
    final val idle            = false
    final val done            = false
    final val failed          = false
    final val idleOrPrepared  = true
    final val stoppedOrDone   = false
  }
  case object Running extends State {
    final val id              = 3
    final val idle            = false
    final val done            = false
    final val failed          = false
    final val idleOrPrepared  = false
    final val stoppedOrDone   = false
  }
  case object Done extends State {
    final val id              = 4
    final val idle            = true
    final val done            = true
    final val failed          = false
    final val idleOrPrepared  = true
    final val stoppedOrDone   = true
  }
  final case class Failed(ex: Throwable) extends State {
    final val id              = 5
    final val idle            = true
    final val done            = false
    final val failed          = true
    final val idleOrPrepared  = true
    final val stoppedOrDone   = false
  }

  def emptyAttr[S <: Sys[S]]: Attr[S] = Context.emptyAttr[S]

//  type Attr = Map[String, Any]
  type Attr[S <: Sys[S]] = Context.Attr[S] // MapLike[S, String, Form]

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def getFactory(tpe: Obj.Type): Option[Factory] = Impl.getFactory(tpe)

  def factories: Iterable[Factory] = Impl.factories

  def get[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, h: Universe[S]): Option[Runner[S]] =
    h.mkRunner(obj)

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, h: Universe[S]): Runner[S] =
    get(obj).getOrElse(throw new IllegalArgumentException(s"No runner factory for ${obj.tpe}"))

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

    def mkRunner[S <: SSys[S]](obj: Repr[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S]
  }

  // -------------------
  // ---- factories ----
  // -------------------

  // ---- Control ----

  object Control extends Factory {
    final val prefix          = "Control"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Control

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Control[~]

    def mkRunner[S <: Sys[S]](obj: _Control[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      ControlRunnerImpl(obj)
  }

  // ---- Action ----

  object Action extends Factory {
    final val prefix          = "Action"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Control

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Action[~]

    def mkRunner[S <: Sys[S]](obj: _Action[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      ActionRunnerImpl(obj)
  }

  // ---- ActionRaw ----

  @deprecated("Action should be used instead of ActionRaw", since = "3.35.3")
  object ActionRaw extends Factory {
    final val prefix          = "ActionRaw"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _ActionRaw

    def isSingleton     : Boolean = false
//    def isInstantaneous : Boolean = true

    type Repr[~ <: Sys[~]] = _ActionRaw[~]

    def mkRunner[S <: Sys[S]](obj: _ActionRaw[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      ActionRawRunnerImpl(obj)
  }

  // ---- Proc ----

  object Proc extends Factory {
    final val prefix          = "Proc"
    final val humanName       = "Process"
    def tpe       : Obj.Type  = _Proc

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Proc[~]

    def mkRunner[S <: SSys[S]](obj: _Proc[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      BasicAuralRunnerImpl(obj)
  }

  // ---- Folder ----

  object Folder extends Factory {
    final val prefix          = "Folder"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Folder

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Folder[~]

    def mkRunner[S <: SSys[S]](obj: _Folder[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      FolderRunnerImpl(obj)
  }

  // ---- Timeline ----

  object Timeline extends Factory {
    final val prefix          = "Timeline"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Timeline

    def isSingleton: Boolean = false

    type Repr[~ <: Sys[~]] = _Timeline[~]

    def mkRunner[S <: SSys[S]](obj: _Timeline[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      TimelineRunnerImpl(obj)
  }

  // ----

  private lazy val fmtMessageDate = new SimpleDateFormat("[HH:mm''ss.SSS]", Locale.US)

  object Message {
    sealed trait Level { def value: Int }
    case object Info    extends Level { final val value = 0 }
    case object Warning extends Level { final val value = 1 }
    case object Error   extends Level { final val value = 2 }
  }
  final case class Message(time: Long, level: Message.Level, text: String) {
    override def toString: String =
      f"${fmtMessageDate.format(new Date(time))} $level%-7s - $text"
  }

  trait Messages[Tx] extends Observable[Tx, List[Message]] {
    def current(implicit tx: Tx): List[Message]
  }

  trait Progress[Tx] extends Observable[Tx, Double] {
    /** Zero to one. Note: negative numbers indicate indeterminate progress */
    def current(implicit tx: Tx): Double
  }

  trait Internal[S <: Sys[S]] extends Runner[S] {
    def completeWith(result: Try[Unit])(implicit tx: S#Tx): Unit

    def setProgress(value: Double)(implicit tx: S#Tx): Unit

    def addMessage(m: Message)(implicit tx: S#Tx): Unit

    def setMessages(m: List[Message])(implicit tx: S#Tx): Unit

    def addDisposable(d: Disposable[S#Tx])(implicit tx: S#Tx): Unit
  }

  // ---- extension methods ----

  implicit final class RunnerOps[S <: Sys[S]](private val r: Runner[S]) extends AnyVal {
    /** Starts the runner, and then watches it until it is stopped
      * (or done or failed), then calling `dispose` on it.
      */
    def runAndDispose()(implicit tx: S#Tx): Unit = {
      r.run()
      r.reactNow { implicit tx => state =>
        if (state.idle) r.dispose()
      }
    }
  }
}
trait Runner[S <: Sys[S]] extends ViewBase[S] with IControl[S] {
  def messages: Runner.Messages[S#Tx] // (implicit tx: S#Tx): Any

  def progress: Runner.Progress[S#Tx]

  implicit val universe: Universe[S]

  def prepare(attr: Runner.Attr[S] = Runner.emptyAttr[S])(implicit tx: S#Tx): Unit

  def run()(implicit tx: S#Tx): Unit

  /*
  - allow both for a `self` and an `invoker` (`Action.Universe`)
  - should we have an `value: Any` as in `Action.Universe`?
  - actually, `invoker` and potential `value` should go into `play` and/or `prepare`
   */
}
