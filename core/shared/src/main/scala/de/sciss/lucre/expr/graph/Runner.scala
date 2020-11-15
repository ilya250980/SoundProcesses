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

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExprAsRunnerMap}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn, synth}
import de.sciss.model.Change
import de.sciss.proc
import de.sciss.proc.{Universe, UGenGraphBuilder => UGB}

import scala.concurrent.stm.Ref

object Runner {
  private final class ExpandedRun[T <: Txn[T]](r: proc.Runner[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      r.run()
  }

  final case class Run(r: Runner) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Runner$$Run" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      new ExpandedRun[T](rx)
    }
  }

  private final class ExpandedRunWith[T <: Txn[T]](r: proc.Runner[T], attr: proc.Runner.Attr[T])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      r.prepare(attr)
      r.run()
    }
  }

  final case class RunWith(r: Runner, map: Seq[Ex[(String, _)]]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Runner$$RunWith" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx    = r.expand[T]
      val mapEx = map.map(_.expand[T])
      val attr  = new IExprAsRunnerMap[T](mapEx, tx)
      new ExpandedRunWith[T](rx, attr)
    }
  }

  private final class ExpandedStop[T <: Txn[T]](r: proc.Runner[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      r.stop()
  }

  final case class Stop(r: Runner) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Runner$$Stop" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      new ExpandedStop[T](rx)
    }
  }

  private final class ExpandedState[T <: Txn[T]](r: proc.Runner[T], tx0: T)
                                                (implicit protected val targets: ITargets[T])
    extends IExpr[T, Int] with IChangeGeneratorEvent[T, Int] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.react { implicit tx => state =>
      val now     = state.id
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Int = r.state.id

    def dispose()(implicit tx: T): Unit = obs.dispose()

    def changed: IChangeEvent[T, Int] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Int =
      pull.resolveExpr(this)
  }

  final case class State(r: Runner) extends Ex[Int] {
    type Repr[T <: Txn[T]] = IExpr[T, Int]

    override def productPrefix: String = s"Runner$$State" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx = r.expand[T]
      new ExpandedState[T](rx, tx)
    }
  }

  private final class ExpandedProgress[T <: Txn[T]](r: proc.Runner[T], tx0: T)
                                                (implicit protected val targets: ITargets[T])
    extends IExpr[T, Double] with IChangeGeneratorEvent[T, Double] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.progress.react { implicit tx => now =>
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Double = r.progress.current

    def dispose()(implicit tx: T): Unit = obs.dispose()

    def changed: IChangeEvent[T, Double] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Double =
      pull.resolveExpr(this)
  }

  final case class Progress(r: Runner) extends Ex[Double] {
    type Repr[T <: Txn[T]] = IExpr[T, Double]

    override def productPrefix: String = s"Runner$$Progress" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx = r.expand[T]
      new ExpandedProgress[T](rx, tx)
    }
  }

  private type Message  = (Long, Int, String)
  private type Msg      = Seq[Message]

//  final case class Message(time: Long, level: Int, text: String) {
//    override def productPrefix: String = s"Runner$$Message" // serialization
//  }

  private final class ExpandedMessages[T <: Txn[T]](r: proc.Runner[T], tx0: T)
                                                   (implicit protected val targets: ITargets[T])
    extends IExpr[T, Msg] with IChangeGeneratorEvent[T, Msg] {

    private[this] val beforeRef = Ref(value(tx0))

    private def msgOf(in: proc.Runner.Message): Message = (in.time, in.level.value, in.text)

    private[this] val obs = r.messages.react { implicit tx => now0 =>
      val now     = now0.map(msgOf)
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Msg = r.messages.current.map(msgOf)

    def dispose()(implicit tx: T): Unit = obs.dispose()

    def changed: IChangeEvent[T, Msg] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Msg =
      pull.resolveExpr(this)
  }

  final case class Messages(r: Runner) extends Ex[Seq[(Long, Int, String)]] {
    type Repr[T <: Txn[T]] = IExpr[T, Seq[(Long, Int, String)]]

    override def productPrefix: String = s"Runner$$Messages" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx = r.expand[T]
      new ExpandedMessages[T](rx, tx)
    }
  }

  def apply(key: String): Runner = Impl(key)

  private final case class Impl(key: String) extends Runner {
    override def productPrefix: String = "Runner" // serialization

    type Repr[T <: Txn[T]] = proc.Runner[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      tx match {
        case stx: synth.Txn[_] =>
          // XXX TODO --- ugly ugly ugly
          mkControlImpl[stx.Ev](ctx.asInstanceOf[Context[stx.Ev]], tx.asInstanceOf[stx.Ev]).asInstanceOf[Repr[T]]

        case _ => throw new Exception("Need a SoundProcesses system")
      }

    private def mkControlImpl[T <: synth.Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.{cursor, workspace}
      val objOpt                  = ctx.selfOption.flatMap(self => self.attr.get(key))
      val obj                     = objOpt.getOrElse(throw UGB.MissingIn(UGB.AttributeKey(key)))
      implicit val h: Universe[T] = Universe()
      proc.Runner[T](obj)
    }
  }
}
trait Runner extends Control {
  // def key: String

  type Repr[T <: Txn[T]] <: proc.Runner[T]

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
