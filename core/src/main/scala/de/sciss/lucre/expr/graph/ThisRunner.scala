/*
 *  ThisRunner.scala
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
import de.sciss.lucre.expr.{Context, Graph, IAction, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc
import de.sciss.synth.proc.ExprContext

import scala.util.{Failure, Success}

object ThisRunner {
  def apply(): ThisRunner = Impl()

  private final class ExpandedStop[S <: Sys[S]](r: proc.Runner[S]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit =
      r.stop()
  }

  final case class Stop(r: ThisRunner) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"ThisRunner$$Stop" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val rx = r.expand[S]
      new ExpandedStop[S](rx)
    }
  }

  private final class ExpandedDone[S <: Sys[S]](r: proc.Runner.Internal[S])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit =
      r.completeWith(Success(()))
  }

  final case class Done(r: ThisRunner) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"ThisRunner$$Done" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val rx = r.expand[S]
      new ExpandedDone[S](rx)
    }
  }

  private final class ExpandedFail[S <: Sys[S]](r: proc.Runner.Internal[S], failure: IExpr[S, String])
    extends IActionImpl[S] {

    def executeAction()(implicit tx: S#Tx): Unit = {
      val failureV  = failure.value
      val tr        = Failure(new Exception(failureV))
      r.completeWith(tr)
    }
  }

  final case class Fail(r: ThisRunner, failure: Ex[String]) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix: String = s"ThisRunner$$Fail" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val rx = r.expand[S]
      val fx = failure.expand[S]
      new ExpandedFail[S](rx, fx)
    }
  }

  private final val defaultProgress = -1.0
//  private final val defaultMessages = List.empty[Message]
  private final val keyProgress     = "progress"
//  private final val keyMessages     = "messages"

//  final case class Progress(r: ThisRunner) extends Ex[Double] {
//    type Repr[S <: Sys[S]] = IExpr[S, Double]
//
//    override def productPrefix: String = s"ThisRunner$$Progress" // serialization
//
//    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
//      val rx          = r.expand[S]
//      val selectedOpt = ctx.getProperty[Ex[Double]](r, keyProgress)
//      val selected0   = selectedOpt.fold[Double](defaultProgress)(_.expand[S].value)
//      import ctx.{cursor, targets}
//      new ProgressExpanded[S](rx, selected0).init()
//    }
//  }

  final case class Progress(r: ThisRunner) extends Ex[Double] {
    type Repr[S <: Sys[S]] = IExpr[S, Double]

    override def productPrefix: String = s"ThisRunner$$Progress" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val valueOpt = ctx.getProperty[Ex[Double]](r, keyProgress)
      valueOpt.getOrElse(Const(defaultProgress)).expand[S]
    }
  }

//  final case class Messages(r: ThisRunner) extends Ex[Seq[Message]] {
//    type Repr[S <: Sys[S]] = IExpr[S, Seq[Message]]
//
//    override def productPrefix: String = s"ThisRunner$$Messages" // serialization
//
//    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
//      val valueOpt = ctx.getProperty[Ex[Seq[Message]]](r, keyMessages)
//      valueOpt.getOrElse(Const(defaultMessages)).expand[S]
//    }
//  }

  private final case class Impl() extends ThisRunner { r =>
    override def productPrefix: String = "ThisRunner" // serialization

    type Repr[S <: Sys[S]] = proc.Runner.Internal[S]

    def stop: Act = ThisRunner.Stop(this)

    def progress: Ex[Double] = Progress(this)

    def progress_=(value: Ex[Double]): Unit = {
      val b = Graph.builder
      b.putProperty(this, keyProgress, value)
    }

//    def messages: Ex[Seq[Message]] = Messages(this)
//
//    def messages_=(value: Ex[Seq[Message]]): Unit = {
//      val b = Graph.builder
//      b.putProperty(this, keyMessages, value)
//    }

    def done: Act = ThisRunner.Done(this)

    def fail(cause: Ex[String]): Act = ThisRunner.Fail(this, cause)

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val ec  = ExprContext.get
      val ri  = ec.runner.getOrElse(sys.error(s"$this - expansion outside of Runner"))
//      ctx.getProperty[Ex[Seq[Message]]](r, keyMessages).foreach { m =>
//        val messagesEx = m.expand[S]
//        val obs = messagesEx.changed.react { implicit tx => messagesCh =>
//          ri.setMessages(messagesCh.now)
//        }
//        ri.addDisposable(obs)
//      }
      ctx.getProperty[Ex[Double]](r, keyProgress).foreach { p =>
        val progressEx = p.expand[S]
        val obs = progressEx.changed.react { implicit tx => progressCh =>
          ri.setProgress(progressCh.now)
        }
        ri.addDisposable(obs)
      }
      ri
    }
  }
}
trait ThisRunner extends Control {
  type Repr[S <: Sys[S]] <: proc.Runner.Internal[S]

  def stop: Act

  def done: Act
  
  def fail(cause: Ex[String]): Act

  var progress: Ex[Double]

  // XXX TODO: proc.Runner.Message is not serializable or constructable from Ex

//  var messages: Ex[Seq[proc.Runner.Message]]
}
