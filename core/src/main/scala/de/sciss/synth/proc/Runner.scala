/*
 *  Runner.scala
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

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.event.Observable
import de.sciss.lucre.expr.IControl
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.impl.{ActionRunnerImpl, BasicAuralRunnerImpl, ControlRunnerImpl, TimelineRunnerImpl, RunnerUniverseImpl => Impl}
import de.sciss.synth.proc.{Action => _Action, Control => _Control, Proc => _Proc, Timeline => _Timeline}

import scala.language.higherKinds

object Runner {
  sealed trait State {
    def id: Int

    /** Stopped or completed. */
    def idle: Boolean

    def idleOrPrepared: Boolean
  }

  /** The initial state of an object and after `stop` has been called and performed.
    * If an object comes to a halt by itself, it will depend on the type of object
    * whether it goes back to `Stopped` or finishes with `Done`.
    */
  case object Stopped extends State {
    final val id              = 0
    final val idle            = true
    final val idleOrPrepared  = true
  }
  case object Preparing extends State {
    final val id              = 1
    final val idle            = false
    final val idleOrPrepared  = false
  }
  case object Prepared extends State {
    final val id              = 2
    /** Note: this reports `false` */
    final val idle            = false
    final val idleOrPrepared  = true
  }
  case object Running extends State {
    final val id              = 3
    final val idle            = false
    final val idleOrPrepared  = false
  }
  case object Done extends State {
    final val id              = 4
    final val idle            = true
    final val idleOrPrepared  = true
  }
  final case class Failed(ex: Throwable) extends State {
    final val id              = 5
    final val idle            = true
    final val idleOrPrepared  = true
  }

  type Attr = Map[String, Any]

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
    def tpe       : Obj.Type  = _Action

    def isSingleton     : Boolean = false
//    def isInstantaneous : Boolean = true

    type Repr[~ <: Sys[~]] = _Action[~]

    def mkRunner[S <: Sys[S]](obj: _Action[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] =
      ActionRunnerImpl(obj)
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
    sealed trait Level
    case object Error   extends Level
    case object Warning extends Level
    case object Info    extends Level
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
}
trait Runner[S <: Sys[S]] extends ViewBase[S] with IControl[S] {
  def messages: Runner.Messages[S#Tx] // (implicit tx: S#Tx): Any

  def progress: Runner.Progress[S#Tx]

  implicit val universe: Universe[S]

  def prepare(attr: Runner.Attr = Map.empty)(implicit tx: S#Tx): Unit

  def run()(implicit tx: S#Tx): Unit

  /*
  - allow both for a `self` and an `invoker` (`Action.Universe`)
  - should we have an `value: Any` as in `Action.Universe`?
  - actually, `invoker` and potential `value` should go into `play` and/or `prepare`
   */
}
