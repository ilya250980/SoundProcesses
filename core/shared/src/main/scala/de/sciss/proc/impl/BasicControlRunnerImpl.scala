/*
 *  BasicControlRunnerImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.edit.UndoManager
import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.{Obj, Source, Txn}
import de.sciss.proc.Runner.{Attr, Failed, Prepared, Preparing, Running, Stopped}
import de.sciss.proc.{ExprContext, Runner, Universe}

import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

abstract class BasicControlRunnerImpl[T <: Txn[T], IC <: IControl[T]](objH: Source[T, Obj[T]])
                                                                     (implicit val universe: Universe[T])
  extends BasicRunnerInternalImpl[T] {

  protected type IRepr = IC

  // ---- abstract ----

  protected def run(tr: Try[IRepr])(implicit tx: T): Unit

  protected def expandGraph()(implicit tx: T, ctx: Context[T]): IRepr

  // ---- impl ----

  private[this] val ctlRef  = Ref(Option.empty[Try[IC]])
  private[this] val attrRef = Ref(Context.emptyAttr[T]) // (NoManifest)

  // XXX TODO --- should unify Runner and ObjViewBase
  // type Repr = Control[T]
  // def tpe: Obj.Type = Control

  override protected def stateWillChanged(now: Runner.State)(implicit tx: T): Unit = {
    if (now.idle) disposeData()
    super.stateWillChanged(now)
  }

  def stop()(implicit tx: T): Unit = {
    //      disposeData()
    state = Stopped
  }

//  override def toString = s"Runner.Control${hashCode().toHexString}"

  override protected def disposeData()(implicit tx: T): Unit = {
    super.disposeData()
    attrRef() = Context.emptyAttr
    disposeRef()
  }

  private def disposeRef()(implicit tx: T): Unit = {
    ctlRef() = None
    //      ctlRef.swap(None) match {
    //        case Some((_, ctx)) =>
    ////          tr.foreach(_.dispose())
    //          ctx.dispose()
    //        case _ =>
    //      }
  }

  def prepare(attr: Attr[T])(implicit tx: T): Unit = {
    def ok(): Unit = {
      attrRef() = attr
      val tr    = mkRef()
      state = tr match {
        case Success(_)   => Prepared
        case Failure(ex)  => Failed(ex)
      }
    }

    state match {
      case s if s.idle  => ok()
      case Prepared     => ()

      case _ => // running or preparing; go back to square one
        // stop()
        // prepare(attr)
        disposeData()
        ok()
    }
  }

  def run()(implicit tx: T): Unit = {
    def ok(): Unit = {
      mkRef()
      runWithRef()
    }

    state match {
      case s if s.idle  => ok()
      case Prepared     => runWithRef()
      case Running      => ()
      case Preparing    => // go back to square one
        disposeData()
        ok()
    }
  }

  private def runWithRef()(implicit tx: T): Unit = {
    val ctlOpt = ctlRef()
    ctlOpt.foreach(run)
  }

  private def mkRef()(implicit tx: T): Try[IControl[T]] = {
    disposeRef()
    implicit val u: UndoManager[T]  = UndoManager()
    val attr  = attrRef()
    implicit val ctx: Context[T]    = ExprContext(Some(objH), attr, Some(this))
    val res   = Try(expandGraph())
    addDisposable(ctx)
    ctlRef()  = Some(res)
    res
  }
}

