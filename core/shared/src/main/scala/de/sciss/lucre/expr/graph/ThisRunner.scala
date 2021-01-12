/*
 *  ThisRunner.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.{Attr => _Attr}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, Graph, IAction, IControl}
import de.sciss.lucre.{Adjunct, IExpr, ProductWithAdjuncts, Txn}
import de.sciss.proc
import de.sciss.proc.ExprContext

import scala.util.{Failure, Success}

object ThisRunner extends ProductReader[ThisRunner] {
  def apply(): ThisRunner = Impl()

  private final class ExpandedStop[T <: Txn[T]](r: proc.Runner[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      r.stop()
  }

  object Stop extends ProductReader[Stop] {
    override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Stop = {
      require (arity == 1 && adjuncts.isEmpty)
      val _r = in.readProductT[ThisRunner]()
      new Stop(_r)
    }
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

  object Done extends ProductReader[Done] {
    override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Done = {
      require (arity == 1 && adjuncts.isEmpty)
      val _r = in.readProductT[ThisRunner]()
      new Done(_r)
    }
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

  object Fail extends ProductReader[Fail] {
    override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Fail = {
      require (arity == 2 && adjuncts.isEmpty)
      val _r        = in.readProductT[ThisRunner]()
      val _failure  = in.readEx[String]()
      new Fail(_r, _failure)
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
  private final val keyProgress     = "progress"

  object Progress extends ProductReader[Progress] {
    override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Progress = {
      require (arity == 1 && adjuncts.isEmpty)
      val _r = in.readProductT[ThisRunner]()
      new Progress(_r)
    }
  }
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

  object Attr extends ProductReader[Attr[_]] {
    object Update extends ProductReader[Update[_]] {
      override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Update[_] = {
        require (arity == 2 && adjuncts.isEmpty)
        val _source = in.readEx[Any]()
        val _key    = in.readString()
        new Update(_source, _key)
      }
    }
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

    object Set extends ProductReader[Set[_]] {
      override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Set[_] = {
        require (arity == 2 && adjuncts.isEmpty)
        val _source = in.readEx[Any]()
        val _key    = in.readString()
        new Set(_source, _key)
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

    override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): Attr[_] = {
      require (arity == 2 && adjuncts.size == 1)
      val _r    = in.readProductT[ThisRunner]()
      val _key  = in.readString()
      val (_bridge: Obj.Bridge[Any]) :: Nil = adjuncts
      new Attr(_r, _key)(_bridge)
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

  override def read(in: RefMapIn, key: String, arity: Int, adjuncts: List[Adjunct]): ThisRunner = {
    require (arity == 0 && adjuncts.isEmpty)
    ThisRunner()
  }

  private final case class Impl() extends ThisRunner { r =>
    override def productPrefix: String = "ThisRunner" // serialization

    type Repr[T <: Txn[T]] = proc.Runner.Internal[T]

    def stop: Act = ThisRunner.Stop(this)

    def progress: Ex[Double] = Progress(this)

    def progress_=(value: Ex[Double]): Unit = {
      val b = Graph.builder
      b.putProperty(this, keyProgress, value)
    }

    def attr[A: Obj.Bridge](key: String): Attr[A] = Attr(this, key)

    def done: Act = ThisRunner.Done(this)

    def fail(cause: Ex[String]): Act = ThisRunner.Fail(this, cause)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val ec  = ExprContext.get
      val ri  = ec.runner.getOrElse(sys.error(s"$this - expansion outside of Runner"))
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
}
