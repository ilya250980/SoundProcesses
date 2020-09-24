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

import de.sciss.lucre.expr.graph.{Attr => _Attr}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, Graph, IAction, IControl}
import de.sciss.lucre.{Adjunct, IExpr, ProductWithAdjuncts, Txn}
import de.sciss.synth.proc
import de.sciss.synth.proc.ExprContext

import scala.util.{Failure, Success}

object ThisRunner {
  def apply(): ThisRunner = Impl()

  private final class ExpandedStop[T <: Txn[T]](r: proc.Runner[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      r.stop()
  }

  final case class Stop(r: ThisRunner) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"ThisRunner$$Stop" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      new ExpandedStop[T](rx)
    }
  }

  private final class ExpandedDone[T <: Txn[T]](r: proc.Runner.Internal[T])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit =
      r.completeWith(Success(()))
  }

  final case class Done(r: ThisRunner) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"ThisRunner$$Done" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      new ExpandedDone[T](rx)
    }
  }

  private final class ExpandedFail[T <: Txn[T]](r: proc.Runner.Internal[T], failure: IExpr[T, String])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val failureV  = failure.value
      val tr        = Failure(new Exception(failureV))
      r.completeWith(tr)
    }
  }

  final case class Fail(r: ThisRunner, failure: Ex[String]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"ThisRunner$$Fail" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      val fx = failure.expand[T]
      new ExpandedFail[T](rx, fx)
    }
  }

  private final val defaultProgress = -1.0
//  private final val defaultMessages = List.empty[Message]
  private final val keyProgress     = "progress"
//  private final val keyMessages     = "messages"

//  final case class Progress(r: ThisRunner) extends Ex[Double] {
//    type Repr[T <: Txn[T]] = IExpr[T, Double]
//
//    override def productPrefix: String = s"ThisRunner$$Progress" // serialization
//
//    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
//      val rx          = r.expand[T]
//      val selectedOpt = ctx.getProperty[Ex[Double]](r, keyProgress)
//      val selected0   = selectedOpt.fold[Double](defaultProgress)(_.expand[T].value)
//      import ctx.{cursor, targets}
//      new ProgressExpanded[T](rx, selected0).init()
//    }
//  }

  final case class Progress(r: ThisRunner) extends Ex[Double] {
    type Repr[T <: Txn[T]] = IExpr[T, Double]

    override def productPrefix: String = s"ThisRunner$$Progress" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val valueOpt = ctx.getProperty[Ex[Double]](r, keyProgress)
      valueOpt.getOrElse(Const(defaultProgress)).expand[T]
    }
  }

  private final class ExpandedUpdateAttr[T <: Txn[T], A, B](source: IExpr[T, A], vr: Var.Expanded[T, B], tx0: T)
    extends IControl[T] {

    private[this] val obs = source.changed.react { implicit tx =>upd =>
      val v = Some(upd.now)
      vr.fromAny.fromAny(v.get).foreach { vT =>
        vr.update(new Const.Expanded(vT))
      }
    } (tx0)

    def dispose()(implicit tx: T): Unit =
      obs.dispose()

    def initControl()(implicit tx: T): Unit = ()
  }

  private final class ExpandedSetAttr[T <: Txn[T], A, B](source: IExpr[T, A], vr: Var.Expanded[T, B])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val v = source.value
      vr.fromAny.fromAny(v).foreach { vT =>
        vr.update(new Const.Expanded(vT))
      }
    }
  }

  object Attr {
    final case class Update[A](source: Ex[A], key: String)
      extends Control {

      override def productPrefix: String = s"ThisRunner$$Attr$$Update"  // serialization

      type Repr[T <: Txn[T]] = IControl[T]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        ctx.attr.get(key) match {
          case Some(vr: Var.Expanded[T, _]) =>
            new ExpandedUpdateAttr(source.expand[T], vr, tx)

          case _ =>
            IControl.empty
        }
      }
    }

    final case class Set[A](source: Ex[A], key: String)
      extends Act {

      override def productPrefix: String = s"ThisRunner$$Attr$$Set"  // serialization

      type Repr[T <: Txn[T]] = IAction[T]

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        ctx.attr.get(key) match {
          case Some(vr: Var.Expanded[T, _]) =>
            new ExpandedSetAttr(source.expand[T], vr)

          case _ =>
            IAction.empty
        }
      }
    }
  }
  final case class Attr[A](r: ThisRunner, key: String)(implicit bridge: Obj.Bridge[A])
    extends Ex[Option[A]] with _Attr.Like[A] with ProductWithAdjuncts {

    override def productPrefix: String = s"ThisRunner$$Attr"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[A]]

    def update(in: Ex[A]): Control  = Attr.Update (in, key)
    def set   (in: Ex[A]): Act      = Attr.Set    (in, key)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rEx     = r.expand[T]
      val ctxFull = bridge.contextCellView(key)
      val res     = new _Attr.Expanded[T, A](key, ctxFull, tx)
      rEx.addDisposable(res)
      res
    }

    def adjuncts: List[Adjunct] = bridge :: Nil
  }

//  final case class Messages(r: ThisRunner) extends Ex[Seq[Message]] {
//    type Repr[T <: Txn[T]] = IExpr[T, Seq[Message]]
//
//    override def productPrefix: String = s"ThisRunner$$Messages" // serialization
//
//    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
//      val valueOpt = ctx.getProperty[Ex[Seq[Message]]](r, keyMessages)
//      valueOpt.getOrElse(Const(defaultMessages)).expand[T]
//    }
//  }

  private final case class Impl() extends ThisRunner { r =>
    override def productPrefix: String = "ThisRunner" // serialization

    type Repr[T <: Txn[T]] = proc.Runner.Internal[T]

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

    def attr[A: Obj.Bridge](key: String): Attr[A] = Attr(this, key)

    def done: Act = ThisRunner.Done(this)

    def fail(cause: Ex[String]): Act = ThisRunner.Fail(this, cause)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val ec  = ExprContext.get
      val ri  = ec.runner.getOrElse(sys.error(s"$this - expansion outside of Runner"))
//      ctx.getProperty[Ex[Seq[Message]]](r, keyMessages).foreach { m =>
//        val messagesEx = m.expand[T]
//        val obs = messagesEx.changed.react { implicit tx => messagesCh =>
//          ri.setMessages(messagesCh.now)
//        }
//        ri.addDisposable(obs)
//      }
      ctx.getProperty[Ex[Double]](r, keyProgress).foreach { p =>
        val progressEx = p.expand[T]
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
  type Repr[T <: Txn[T]] <: proc.Runner.Internal[T]

  def stop: Act

  def done: Act

  def fail(cause: Ex[String]): Act

  var progress: Ex[Double]

  /** A runner attribute that can be read or updated (if the runner was called with a `Var`).
    * This does not fall back to the self object's attribute map, which would be the case
    * for `key.attr[A]`.
    */
  def attr[A: Obj.Bridge](key: String): ThisRunner.Attr[A]

  // XXX TODO: proc.Runner.Message is not serializable or constructable from Ex

//  var messages: Ex[Seq[proc.Runner.Message]]
}
