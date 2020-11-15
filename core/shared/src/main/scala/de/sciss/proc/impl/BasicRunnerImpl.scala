/*
 *  BasicRunnerImpl.scala
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

package de.sciss.proc.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.{Cursor, Disposable, Obj, Source, Txn, Workspace, synth}
import de.sciss.proc.Runner.{Attr, Preparing, Running, State, Stopped}
import de.sciss.proc.impl.BasicAuralRunnerImpl.AuralRef
import de.sciss.proc.{AuralContext, AuralObj, Runner, TimeRef, Universe}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref
import scala.util.{Failure, Success, Try}

trait BasicRunnerImpl[T <: Txn[T]]
  extends Runner[T] with BasicViewBaseImpl[T] {

  // ---- abstract ----

  protected def disposeData()(implicit tx: T): Unit

  // ---- impl ----

  implicit final def workspace : Workspace[T] = universe.workspace
  implicit final def cursor    : Cursor[T]    = universe.cursor

  def initControl()(implicit tx: T): Unit = ()

  // this is implemented so there is no chance of forgetting
  // to remove the runner from the handler
  final def dispose()(implicit tx: T): Unit = {
    universe.removeRunner(this)
    disposeData()
  }

  final object messages extends Runner.Messages[T] with ObservableImpl[T, List[Runner.Message]] {
    private[this] val ref = Ref(List.empty[Runner.Message])

    def current(implicit tx: T): List[Runner.Message] = ref()

    def current_=(value: List[Runner.Message])(implicit tx: T): Unit = {
      val old = ref.swap(value)
      if (value !== old) fire(value)
    }
  }
}

trait BasicRunnerInternalImpl[T <: Txn[T]]
  extends BasicRunnerImpl[T] with Runner.Internal[T] {

  private[this] val disposables = Ref(List.empty[Disposable[T]])

  object progress extends Runner.Progress[T] with ObservableImpl[T, Double] {
    private[this] val ref = Ref(-1.0)

    def current(implicit tx: T): Double = ref()

    def current_=(value: Double)(implicit tx: T): Unit = {
      val old = ref.swap(value)
      if (value !== old) fire(value)
    }
  }

  protected def disposeData()(implicit tx: T): Unit =
    disposables.swap(Nil).foreach(_.dispose())

  def completeWith(result: Try[Unit])(implicit tx: T): Unit = {
    state = result match {
      case Success(_)   => Runner.Done
      case Failure(ex)  => Runner.Failed(ex)
    }
  }

  def setProgress(value: Double)(implicit tx: T): Unit =
    progress.current = value

  def addMessage(m: Runner.Message)(implicit tx: T): Unit =
    messages.current = messages.current :+ m

  def setMessages(m: List[Runner.Message])(implicit tx: T): Unit =
    messages.current = m

  def addDisposable(d: Disposable[T])(implicit tx: T): Unit =
    disposables.transform(d :: _)
}

/** An implementation that maintains an `AuralObj` of the object which is run and stopped. */
trait BasicAuralRunnerImpl[T <: synth.Txn[T]] extends AuralSystemTxBridge[T] with BasicRunnerImpl[T] {
//  def initRunner(obj: Obj[T])(implicit tx: T): this.type = {
//    this
//  }

  protected def obj(implicit tx: T): Obj[T]

  private[this] val targetState     = Ref[Runner.State](Runner.Stopped)
  private[this] val auralRef        = Ref(Option.empty[AuralRef[T]])
  private[this] val contextRef      = Ref(Option.empty[AuralContext[T]])
  private[this] val attrRef         = Ref(Runner.emptyAttr[T]) // (NoManifest)
  private[this] val attrDirty       = Ref(false)

  object progress extends Runner.Progress[T] with DummyObservableImpl[T] {
    def current(implicit tx: T): Double = -1
  }

  def prepare(attr: Attr[T])(implicit tx: T): Unit = {
    val oldAttr = attrRef.swap(attr)
    attrDirty() = oldAttr !== attr
    setAndMatchStates(Preparing)
  }

  def run()(implicit tx: T): Unit =
    setAndMatchStates(Running)

  def stop()(implicit tx: T): Unit =
    setAndMatchStates(Stopped)

  private def disposeRef()(implicit tx: T): Unit =
    auralRef.swap(None).foreach(_.dispose())

  private def mkRef()(implicit tx: T): Option[AuralRef[T]] = {
    disposeRef()
    state = Stopped
    val newOpt = contextRef().map { implicit ctx =>
      val view  = AuralObj(obj, attr = attrRef())
      val obs   = view.react { implicit tx => viewState =>
        // This is quite tricky:
        // If the aural-view stops (itself, or errors), that is it
        // goes from non-stopped to stopped or failed, then it should
        // not matter what the target state is (it is probably
        // `Playing`); instead, we should interpret this as
        // the view setting the new target state.
        // On the other hand, if the view goes to `Prepared`,
        // and the target is `Playing`, then we should stick
        // to the target and match the states.
        if (viewState.idle) {
          targetState() = viewState
        }
        state = viewState
        matchStates()
      }
      new AuralRef(view, obs)
    }
    auralRef()  = newOpt
    attrDirty() = false
    newOpt
  }

  private def setAndMatchStates(tgt: State)(implicit tx: T): Unit = {
    targetState() = tgt
    matchStates()
  }

  @tailrec
  private def matchStates()(implicit tx: T): Unit = {
    val tgt = targetState()
    auralRef() match {
      case Some(ref) =>
        val view  = ref.view
        val src   = view.state
        if (tgt !== src) tgt match {
          case Stopped => view.stop()
          case Preparing | Running =>
            val dirty = attrDirty()
            if (dirty || (src !== Preparing)) {
              if (!src.idleOrPrepared) {
                view.stop()
              }
              if (!dirty) {
                if (tgt == Running) view.play() else view.prepare(TimeRef.Undefined)
              } else {
                mkRef()
                matchStates()
              }
            }

          case _ => assert(assertion = false, tgt.toString)
        }
      case None =>
        tgt match {
          case Preparing | Running =>
            val newOpt = mkRef()
            if (newOpt.isDefined) matchStates()
          case _ =>
        }
    }
  }

  protected def auralStartedTx()(implicit tx: T, auralContext: AuralContext[T]): Unit = {
    contextRef() = Some(auralContext)
    if (targetState() !== Stopped) {
      matchStates()
    }
  }

  protected def auralStoppedTx()(implicit tx: T): Unit = {
    contextRef() = None
    disposeRef()
    if (targetState() === Stopped) {
      // we are supposed to be stopped; there is no more view,
      // so let's set the dispatched state directly
      state = Stopped
    }
  }

  protected def disposeData()(implicit tx: T): Unit = {
    disconnectAuralSystem()
    disposeRef()
  }
}

object BasicAuralRunnerImpl {
  def apply[T <: synth.Txn[T]](obj: Obj[T])(implicit tx: T, universe: Universe[T]): Runner[T] = {
    new Impl(tx.newHandle(obj), obj.tpe, universe).init()
  }

  private final class AuralRef[T <: synth.Txn[T]](val view: AuralObj[T], observer: Disposable[T]) {
    def dispose()(implicit tx: T): Unit = {
      observer.dispose()
      view    .dispose()
    }
  }

  private final class Impl[T <: synth.Txn[T]](objH: Source[T, Obj[T]], tpe: Obj.Type,
                                         val universe: Universe[T])
    extends BasicAuralRunnerImpl[T] {

    override def toString = s"Runner(${tpe.typeId})@{hashCode().toHexString}"

    protected def obj(implicit tx: T): Obj[T] = objH()

    def init()(implicit tx: T): this.type =
      connectAuralSystem()
  }
}