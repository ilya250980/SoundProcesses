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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.impl.IChangeGenerator
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr, IExprAsRunnerMap}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth
import de.sciss.model.Change
import de.sciss.synth.proc
import de.sciss.synth.proc.{Universe, UGenGraphBuilder => UGB}

import scala.concurrent.stm.Ref

object Runner {
  private final class ExpandedRun[S <: Sys[S]](r: proc.Runner[S]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit =
      r.run()
  }

  final case class Run(r: Runner) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"Runner$$Run" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val rx = r.expand[S]
      new ExpandedRun[S](rx)
    }
  }

  private final class ExpandedRunWith[S <: Sys[S]](r: proc.Runner[S], attr: proc.Runner.Attr[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      r.prepare(attr)
      r.run()
    }
  }

  final case class RunWith(r: Runner, map: Seq[Ex[(String, _)]]) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"Runner$$RunWith" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      val rx    = r.expand[S]
      val mapEx = map.map(_.expand[S])
      val attr  = new IExprAsRunnerMap[S](mapEx, tx)
      new ExpandedRunWith[S](rx, attr)
    }
  }

  private final class ExpandedStop[S <: Sys[S]](r: proc.Runner[S]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit =
      r.stop()
  }

  final case class Stop(r: Runner) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"Runner$$Stop" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val rx = r.expand[S]
      new ExpandedStop[S](rx)
    }
  }

  private final class ExpandedState[S <: Sys[S]](r: proc.Runner[S], tx0: S#Tx)
                                                (implicit protected val targets: ITargets[S])
    extends IExpr[S, Int] with IChangeGenerator[S, Int] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.react { implicit tx => state =>
      val now     = state.id
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Int = r.state.id

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

    def changed: IChangeEvent[S, Int] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Int =
      pull.resolveExpr(this)
  }

  final case class State(r: Runner) extends Ex[Int] {
    type Repr[S <: Sys[S]] = IExpr[S, Int]

    override def productPrefix: String = s"Runner$$State" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      val rx = r.expand[S]
      new ExpandedState[S](rx, tx)
    }
  }

  private final class ExpandedProgress[S <: Sys[S]](r: proc.Runner[S], tx0: S#Tx)
                                                (implicit protected val targets: ITargets[S])
    extends IExpr[S, Double] with IChangeGenerator[S, Double] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.progress.react { implicit tx => now =>
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Double = r.progress.current

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

    def changed: IChangeEvent[S, Double] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Double =
      pull.resolveExpr(this)
  }

  final case class Progress(r: Runner) extends Ex[Double] {
    type Repr[S <: Sys[S]] = IExpr[S, Double]

    override def productPrefix: String = s"Runner$$Progress" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      val rx = r.expand[S]
      new ExpandedProgress[S](rx, tx)
    }
  }

  private type Message  = (Long, Int, String)
  private type Msg      = Seq[Message]

//  final case class Message(time: Long, level: Int, text: String) {
//    override def productPrefix: String = s"Runner$$Message" // serialization
//  }

  private final class ExpandedMessages[S <: Sys[S]](r: proc.Runner[S], tx0: S#Tx)
                                                   (implicit protected val targets: ITargets[S])
    extends IExpr[S, Msg] with IChangeGenerator[S, Msg] {

    private[this] val beforeRef = Ref(value(tx0))

    private def msgOf(in: proc.Runner.Message): Message = (in.time, in.level.value, in.text)

    private[this] val obs = r.messages.react { implicit tx => now0 =>
      val now     = now0.map(msgOf)
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Msg = r.messages.current.map(msgOf)

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

    def changed: IChangeEvent[S, Msg] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Msg =
      pull.resolveExpr(this)
  }

  final case class Messages(r: Runner) extends Ex[Seq[(Long, Int, String)]] {
    type Repr[S <: Sys[S]] = IExpr[S, Seq[(Long, Int, String)]]

    override def productPrefix: String = s"Runner$$Messages" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      val rx = r.expand[S]
      new ExpandedMessages[S](rx, tx)
    }
  }

  def apply(key: String): Runner = Impl(key)

  private final case class Impl(key: String) extends Runner {
    override def productPrefix: String = "Runner" // serialization

    type Repr[S <: Sys[S]] = proc.Runner[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      tx.system match {
        case _: synth.Sys[_] =>
          // XXX TODO --- ugly ugly ugly
          mkControlImpl[synth.NoSys](ctx.asInstanceOf[Context[synth.NoSys]], tx.asInstanceOf[synth.NoSys#Tx])
            .asInstanceOf[Repr[S]]

        case _ => throw new Exception("Need a SoundProcesses system")
      }

    private def mkControlImpl[S <: synth.Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.{cursor, workspace}
      val objOpt                  = ctx.selfOption.flatMap(self => self.attr.get(key))
      val obj                     = objOpt.getOrElse(throw UGB.MissingIn(UGB.AttributeKey(key)))
      implicit val h: Universe[S] = Universe()
      proc.Runner[S](obj)
    }
  }
}
trait Runner extends Control {
  // def key: String

  type Repr[S <: Sys[S]] <: proc.Runner[S]

  def run : Act = Runner.Run  (this)
  def stop: Act = Runner.Stop (this)

  def runWith(attr: Ex[(String, _)]*): Act = Runner.RunWith(this, attr)

//  def runWith(attr: (String, Ex[_])*): Act = Runner.RunWith(this, attr)

  /** 0 - stopped, 1 - preparing, 2 - prepared, 3 - running, 4 - done, 5 - failed */
  def state: Ex[Int] = Runner.State(this)

  /** Triggers if the state becomes 0 */
  def stopped       : Trig = (state sig_== 0).toTrig

  /** Triggers if the state becomes 4 */
  def done          : Trig = (state sig_== 4).toTrig

  /** Triggers if the state becomes 5 */
  def failed        : Trig = (state sig_== 5).toTrig

  /** Triggers if the state becomes 0 or 4 */
  def stoppedOrDone : Trig = {
    val s = state
    ((s sig_== 0) || (s sig_== 4)).toTrig
  }

  /** Triggers if the state becomes 0, 4, or 5 */
  def idle          : Trig = {
    val s = state
    ((s sig_== 0) || (s >= 4)).toTrig
  }

  /** Zero to one. Negative if unknown */
  def progress: Ex[Double] = Runner.Progress(this)

  /** A message tuple is (time, level, text) where level is 0 (info), 1 (warn) or 2 (error). */
  def messages: Ex[Seq[(Long, Int, String)]] = Runner.Messages(this)
}
