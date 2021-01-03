/*
 *  Runner.scala
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

package de.sciss.proc

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.{Disposable, Obj, Observable, Txn, synth, Folder => _Folder}
import de.sciss.proc.impl.{ActionRunnerImpl, BasicAuralRunnerImpl, ControlRunnerImpl, FolderRunnerImpl, TimelineRunnerImpl, RunnerUniverseImpl => Impl}
import de.sciss.proc.{Action => _Action, Control => _Control, Proc => _Proc, Timeline => _Timeline}

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

  def emptyAttr[T <: Txn[T]]: Attr[T] = Context.emptyAttr[T]

//  type Attr = Map[String, Any]
  type Attr[T <: Txn[T]] = Context.Attr[T] // MapLike[T, String, Form]

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def getFactory(tpe: Obj.Type): Option[Factory] = Impl.getFactory(tpe)

  def factories: Iterable[Factory] = Impl.factories

  def get[T <: Txn[T]](obj: Obj[T])(implicit tx: T, h: Universe[T]): Option[Runner[T]] =
    h.mkRunner(obj)

  def apply[T <: Txn[T]](obj: Obj[T])(implicit tx: T, h: Universe[T]): Runner[T] =
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

    type Repr[~ <: Txn[~]] <: Obj[~]

    def mkRunner[T <: synth.Txn[T]](obj: Repr[T])(implicit tx: T, universe: Universe[T]): Runner[T]
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

    type Repr[~ <: Txn[~]] = _Control[~]

    def mkRunner[T <: Txn[T]](obj: _Control[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
      ControlRunnerImpl(obj)
  }

  // ---- Action ----

  object Action extends Factory {
    final val prefix          = "Action"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Control

    def isSingleton: Boolean = false

    type Repr[~ <: Txn[~]] = _Action[~]

    def mkRunner[T <: Txn[T]](obj: _Action[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
      ActionRunnerImpl(obj)
  }

  // ---- Proc ----

  object Proc extends Factory {
    final val prefix          = "Proc"
    final val humanName       = "Process"
    def tpe       : Obj.Type  = _Proc

    def isSingleton: Boolean = false

    type Repr[~ <: Txn[~]] = _Proc[~]

    def mkRunner[T <: synth.Txn[T]](obj: _Proc[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
      BasicAuralRunnerImpl(obj)
  }

  // ---- Folder ----

  object Folder extends Factory {
    final val prefix          = "Folder"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Folder

    def isSingleton: Boolean = false

    type Repr[~ <: Txn[~]] = _Folder[~]

    def mkRunner[T <: synth.Txn[T]](obj: _Folder[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
      FolderRunnerImpl(obj)
  }

  // ---- Timeline ----

  object Timeline extends Factory {
    final val prefix          = "Timeline"
    def humanName : String    = prefix
    def tpe       : Obj.Type  = _Timeline

    def isSingleton: Boolean = false

    type Repr[~ <: Txn[~]] = _Timeline[~]

    def mkRunner[T <: synth.Txn[T]](obj: _Timeline[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
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

  trait Internal[T <: Txn[T]] extends Runner[T] {
    def completeWith(result: Try[Unit])(implicit tx: T): Unit

    def setProgress(value: Double)(implicit tx: T): Unit

    def addMessage(m: Message)(implicit tx: T): Unit

    def setMessages(m: List[Message])(implicit tx: T): Unit

    def addDisposable(d: Disposable[T])(implicit tx: T): Unit
  }

  // ---- extension methods ----

  implicit final class RunnerOps[T <: Txn[T]](private val r: Runner[T]) extends AnyVal {
    /** Starts the runner, and then watches it until it is stopped
      * (or done or failed), then calling `dispose` on it.
      */
    def runAndDispose()(implicit tx: T): Unit = {
      r.run()
      r.reactNow { implicit tx => state =>
        if (state.idle) r.dispose()
      }
      ()
    }
  }
}
trait Runner[T <: Txn[T]] extends ViewBase[T] with IControl[T] {
  def messages: Runner.Messages[T] // (implicit tx: T): Any

  def progress: Runner.Progress[T]

  implicit val universe: Universe[T]

  def prepare(attr: Runner.Attr[T] = Runner.emptyAttr[T])(implicit tx: T): Unit

  def run()(implicit tx: T): Unit

  /*
  - allow both for a `self` and an `invoker` (`Action.Universe`)
  - should we have an `value: Any` as in `Action.Universe`?
  - actually, `invoker` and potential `value` should go into `play` and/or `prepare`
   */
}
