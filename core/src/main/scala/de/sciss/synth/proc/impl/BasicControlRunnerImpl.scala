package de.sciss.synth.proc.impl

import de.sciss.lucre.expr.{Context, IControl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Obj, Sys, UndoManager}
import de.sciss.synth.proc.Runner.{Attr, Failed, Prepared, Preparing, Running, Stopped}
import de.sciss.synth.proc.{ExprContext, Runner, Universe}

import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

abstract class BasicControlRunnerImpl[S <: Sys[S], IC <: IControl[S]](objH: stm.Source[S#Tx, Obj[S]])
                                                                     (implicit val universe: Universe[S])
  extends BasicRunnerInternalImpl[S] {

  protected type IRepr = IC

  // ---- abstract ----

  protected def run(tr: Try[IRepr])(implicit tx: S#Tx): Unit

  protected def expandGraph()(implicit tx: S#Tx, ctx: Context[S]): IRepr

  // ---- impl ----

  private[this] val ctlRef  = Ref(Option.empty[Try[IC]])
  private[this] val attrRef = Ref(Context.emptyAttr[S])(NoManifest)

  // XXX TODO --- should unify Runner and ObjViewBase
  // type Repr = Control[S]
  // def tpe: Obj.Type = Control

  override protected def stateWillChanged(now: Runner.State)(implicit tx: S#Tx): Unit = {
    if (now.idle) disposeData()
    super.stateWillChanged(now)
  }

  def stop()(implicit tx: S#Tx): Unit = {
    //      disposeData()
    state = Stopped
  }

//  override def toString = s"Runner.Control${hashCode().toHexString}"

  override protected def disposeData()(implicit tx: S#Tx): Unit = {
    super.disposeData()
    attrRef() = Context.emptyAttr
    disposeRef()
  }

  private def disposeRef()(implicit tx: S#Tx): Unit = {
    ctlRef() = None
    //      ctlRef.swap(None) match {
    //        case Some((_, ctx)) =>
    ////          tr.foreach(_.dispose())
    //          ctx.dispose()
    //        case _ =>
    //      }
  }

  def prepare(attr: Attr[S])(implicit tx: S#Tx): Unit = {
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

  def run()(implicit tx: S#Tx): Unit = {
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

  private def runWithRef()(implicit tx: S#Tx): Unit = {
    val ctlOpt = ctlRef()
    ctlOpt.foreach(run)
  }

  private def mkRef()(implicit tx: S#Tx): Try[IControl[S]] = {
    disposeRef()
    implicit val u: UndoManager[S]  = UndoManager()
    val attr  = attrRef()
    implicit val ctx: Context[S]    = ExprContext(Some(objH), attr, Some(this))
    val res   = Try(expandGraph())
    addDisposable(ctx)
    ctlRef()  = Some(res)
    res
  }
}

