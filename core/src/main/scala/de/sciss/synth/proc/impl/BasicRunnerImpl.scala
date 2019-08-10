/*
 *  BasicRunnerImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.stm
import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Sys, Workspace}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.Runner.{Attr, Preparing, Running, State, Stopped}
import de.sciss.synth.proc.impl.BasicAuralRunnerImpl.AuralRef
import de.sciss.synth.proc.{AuralContext, AuralObj, Runner, TimeRef, Universe}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

trait BasicRunnerImpl[S <: Sys[S]]
  extends Runner[S] with BasicViewBaseImpl[S] {

  // ---- abstract ----

  protected def disposeData()(implicit tx: S#Tx): Unit

  // ---- impl ----

  implicit final def workspace : Workspace[S] = universe.workspace
  implicit final def cursor    : Cursor[S]    = universe.cursor

  def initControl()(implicit tx: S#Tx): Unit = ()


  // this is implemented so there is no chance of forgetting
  // to remove the runner from the handler
  final def dispose()(implicit tx: S#Tx): Unit = {
    universe.removeRunner(this)
    disposeData()
  }

  final object messages extends Runner.Messages[S#Tx] with ObservableImpl[S, List[Runner.Message]] {
    private[this] val ref = Ref(List.empty[Runner.Message])

    def current(implicit tx: S#Tx): List[Runner.Message] = ref()

    def current_=(value: List[Runner.Message])(implicit tx: S#Tx): Unit = {
      ref() = value
      fire(value)
    }
  }
}

/** An implementation that maintains an `AuralObj` of the object which is run and stopped. */
trait BasicAuralRunnerImpl[S <: SSys[S]] extends AuralSystemTxBridge[S] with BasicRunnerImpl[S] {
//  def initRunner(obj: Obj[S])(implicit tx: S#Tx): this.type = {
//    this
//  }

  protected def obj(implicit tx: S#Tx): Obj[S]

  private[this] val targetState     = Ref[Runner.State](Runner.Stopped)
  private[this] val auralRef        = Ref(Option.empty[AuralRef[S]])
  private[this] val contextRef      = Ref(Option.empty[AuralContext[S]])
  private[this] val attrRef         = Ref(Map.empty: Attr)
  private[this] val attrDirty       = Ref(false)

  object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
    def current(implicit tx: S#Tx): Double = -1
  }

  def prepare(attr: Attr)(implicit tx: S#Tx): Unit = {
    val oldAttr = attrRef.swap(attr)
    attrDirty() = oldAttr !== attr
    setAndMatchStates(Preparing)
  }

  def run()(implicit tx: S#Tx): Unit =
    setAndMatchStates(Running)

  def stop()(implicit tx: S#Tx): Unit =
    setAndMatchStates(Stopped)

  private def disposeRef()(implicit tx: S#Tx): Unit =
    auralRef.swap(None).foreach(_.dispose())

  private def mkRef()(implicit tx: S#Tx): Option[AuralRef[S]] = {
    disposeRef()
    state = Stopped
    val newOpt = contextRef().map { implicit ctx =>
      val view  = AuralObj(obj, attr = attrRef())
      val obs   = view.react { implicit tx => viewState =>
        state = viewState
        matchStates()
      }
      new AuralRef(view, obs)
    }
    auralRef()  = newOpt
    attrDirty() = false
    newOpt
  }

  private def setAndMatchStates(tgt: State)(implicit tx: S#Tx): Unit = {
    targetState() = tgt
    matchStates()
  }

  @tailrec
  private def matchStates()(implicit tx: S#Tx): Unit = {
    val tgt = targetState()
    auralRef() match {
      case Some(ref) =>
        val view  = ref.view
        val src   = view.state
        if (tgt !== src) tgt match {
          case Stopped => view.stop()
          case Preparing | Running =>
            if (!src.idleOrPrepared) {
              view.stop()
            }
            if (!attrDirty()) {
              if (tgt == Running) view.play() else view.prepare(TimeRef.Undefined)
            } else {
              mkRef()
              matchStates()
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

  protected def auralStartedTx()(implicit tx: S#Tx, auralContext: AuralContext[S]): Unit = {
    contextRef() = Some(auralContext)
    if (targetState() !== Stopped) {
      matchStates()
    }
  }

  protected def auralStoppedTx()(implicit tx: S#Tx): Unit = {
    contextRef() = None
    disposeRef()
    if (targetState() === Stopped) {
      // we are supposed to be stopped; there is no more view,
      // so let's set the dispatched state directly
      state = Stopped
    }
  }

  protected def disposeData()(implicit tx: S#Tx): Unit = {
    disconnectAuralSystem()
    disposeRef()
  }
}

object BasicAuralRunnerImpl {
  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] = {
    new Impl(tx.newHandle(obj), obj.tpe, universe).init()
  }

  private final class AuralRef[S <: SSys[S]](val view: AuralObj[S], observer: Disposable[S#Tx]) {
    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      view    .dispose()
    }
  }

  private final class Impl[S <: SSys[S]](objH: stm.Source[S#Tx, Obj[S]], tpe: Obj.Type,
                                         val universe: Universe[S])
    extends BasicAuralRunnerImpl[S] {

    override def toString = s"Runner(${tpe.typeId})@{hashCode().toHexString}"

    protected def obj(implicit tx: S#Tx): Obj[S] = objH()

    def init()(implicit tx: S#Tx): this.type =
      connectAuralSystem()
  }
}