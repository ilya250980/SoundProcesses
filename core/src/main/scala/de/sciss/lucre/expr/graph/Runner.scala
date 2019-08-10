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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.impl.IGenerator
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth
import de.sciss.model.Change
import de.sciss.synth.proc
import de.sciss.synth.proc.{Universe, UGenGraphBuilder => UGB}

import scala.concurrent.stm.Ref
import scala.language.higherKinds

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

  private final class ExpandedRunWith[S <: Sys[S]](r: proc.Runner[S], map: IExpr[S, Seq[(String, _)]])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      r.prepare(map.value.toMap)  // XXX TODO --- proc.Runner should take mutable object
      r.run()
    }
  }

  final case class RunWith(r: Runner, map: Ex[Seq[(String, _)]]) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"Runner$$RunWith" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val rx    = r   .expand[S]
      val mapEx = map .expand[S]
      new ExpandedRunWith[S](rx, mapEx)
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
    extends IExpr[S, Int] with IGenerator[S, Change[Int]] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.react { implicit tx => state =>
      val now     = state.id
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Int = r.state.id

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

    def changed: IEvent[S, Change[Int]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Int]] =
      Some(pull.resolve)
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
    extends IExpr[S, Double] with IGenerator[S, Change[Double]] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.progress.react { implicit tx => now =>
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Double = r.progress.current

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

    def changed: IEvent[S, Change[Double]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Double]] =
      Some(pull.resolve)
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

  private type Msg = Seq[proc.Runner.Message]

  private final class ExpandedMessages[S <: Sys[S]](r: proc.Runner[S], tx0: S#Tx)
                                                   (implicit protected val targets: ITargets[S])
    extends IExpr[S, Msg] with IGenerator[S, Change[Msg]] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.messages.react { implicit tx => now =>
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: S#Tx): Msg = r.messages.current

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()

    def changed: IEvent[S, Change[Msg]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[Msg]] =
      Some(pull.resolve)
  }

  final case class Messages(r: Runner) extends Ex[Seq[proc.Runner.Message]] {
    type Repr[S <: Sys[S]] = IExpr[S, Seq[proc.Runner.Message]]

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

  // XXX TODO: should we introduce `Map` at some point?
  def runWith(map: Ex[Seq[(String, _)]]): Act = Runner.RunWith(this, map)

  /** 0 - stopped, 1 - preparing, 2 - prepared, 3 - running */
  def state: Ex[Int] = Runner.State(this)

  /** Zero to one. Negative if unknown */
  def progress: Ex[Double] = Runner.Progress(this)

  def messages: Ex[Seq[proc.Runner.Message]] = Runner.Messages(this)
}
