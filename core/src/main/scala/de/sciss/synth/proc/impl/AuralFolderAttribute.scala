/*
 *  AuralFolderAttribute.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Folder, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, Target}
import de.sciss.synth.proc.Runner.{Prepared, Preparing, Running, Stopped}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, Runner, TimeRef}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object AuralFolderAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Folder[S]

  def tpe: Obj.Type = Folder

  def apply[S <: Sys[S]](key: String, value: Folder[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new AuralFolderAttribute(key, tx.newHandle(value), observer).init(value)


  private sealed trait InternalState[S <: Sys[S]] extends Disposable[S#Tx] {
    def external: Runner.State
  }

  private final case class IStopped[S <: Sys[S]]() extends InternalState[S] {
    def dispose()(implicit tx: S#Tx): Unit = ()
    def external: Runner.State = Stopped
  }

  private final case class IPreparing[S <: Sys[S]](map: Map[AuralAttribute[S], Disposable[S#Tx]], timeRef: TimeRef)
    extends InternalState[S] {

    def dispose()(implicit tx: S#Tx): Unit = map.foreach(_._2.dispose())

    def external: Runner.State = if (map.isEmpty) Prepared else Preparing
  }

  private final case class IPlaying[S <: Sys[S]](wallClock: Long, timeRef: TimeRef, target: Target[S])
    extends InternalState[S] {

    def shiftTo(newWallClock: Long): TimeRef = timeRef.shift(newWallClock - wallClock)

    def dispose()(implicit tx: S#Tx): Unit = ()

    def external: Runner.State = Running
  }
}
final class AuralFolderAttribute[S <: Sys[S]](val key: String, objH: stm.Source[S#Tx, Folder[S]],
                                              observer: Observer[S])
                                             (implicit context: AuralContext[S])
  extends AuralAttribute[S] with ObservableImpl[S, Runner.State] with AuralAttribute.Observer[S] { attr =>

  import TxnLike.peer
//  import context.{scheduler => sched}
  import context.universe.{scheduler => sched}

  type Elem = AuralAttribute[S]

  def tpe: Obj.Type = Folder

  type Repr = Folder[S]

  import AuralFolderAttribute.{IPlaying, IPreparing, IStopped, InternalState}

  def obj(implicit tx: S#Tx): Folder[S] = objH()

  private[this] val childAttrRef  = Ref.make[Vector[Elem]]
  private[this] val _IStopped     = IStopped[S]()
  private[this] val internalRef   = Ref[InternalState[S]](_IStopped)
  private[this] val prefChansRef  = Ref(-2)    // -2 = cache invalid

  private[this] var obs: Disposable[S#Tx] = _

  def targetOption(implicit tx: S#Tx): Option[Target[S]] = internalRef() match {
    case IPlaying(_, _, target) => Some(target)
    case _                      => None
  }

  def preferredNumChannels(implicit tx: S#Tx): Int = {
    @tailrec
    def loop(views: Vector[Elem], res: Int): Int = views match {
      case head +: tail =>
        val ch = head.preferredNumChannels
        // if there is any child with `-1` (undefined), we have to return
        if (ch == -1) ch else loop(tail, math.max(res, ch))
      case _ => res
    }

    val cache = prefChansRef()
    if (cache > -2) cache else {
      val res = loop(childAttrRef(), -1)
      prefChansRef() = res
      res
    }
  }

  // simply forward, for now we don't go into the details of checking
  // `preferredNumChannels`
  def attrNumChannelsChanged(attr: Elem)(implicit tx: S#Tx): Unit =
    invalidateNumChans()

  private def invalidateNumChans()(implicit tx: S#Tx): Unit = {
    prefChansRef() = -2
    observer.attrNumChannelsChanged(this)
  }

  private def mkView(child: Obj[S])(implicit tx: S#Tx): Elem =
    AuralAttribute(key, child, attr)

  def init(folder: Folder[S])(implicit tx: S#Tx): this.type = {
    val childViews = folder.iterator.map { elem =>
      mkView(elem)
    } .toVector
    childAttrRef() = childViews

    // views.foreach(_.init())
    obs = folder.changed.react { implicit tx => upd => upd.changes.foreach {
      case stm.List.Added  (idx, child) =>
        val childView = mkView(child)
        childAttrRef.transform(_.patch(idx, childView :: Nil, 0))
        internalRef() match {
          case play: IPlaying[S] =>
            val tForce = play.shiftTo(sched.time)
            childView.run(tForce, play.target)

          case prep: IPreparing[S] =>
            val prepOpt = prepareChild(childView, prep.timeRef)
            prepOpt.foreach { case (_, childObs) =>
              val map1  = prep.map + (childView -> childObs)
              val prep1 = prep.copy(map = map1)
              internalRef() = prep1
              val st0 = prep.external
              if (st0 == Prepared) fire(Preparing)
            }

          case _ => // Karl-Friedrich von der Stoppenweide
        }

      case stm.List.Removed(idx, _ /* child */) =>
        val c0          = childAttrRef()
        val childView   = c0(idx)
        val childChans  = childView.preferredNumChannels
        childView.dispose()
        val c1          = c0.patch(idx, Nil, 1)
        childAttrRef()  = c1
        childPreparedOrRemoved(childView) // might change state to `Prepared`
        if (childChans == -1 && c1.nonEmpty) invalidateNumChans()
    }}
    this
  }

  def state(implicit tx: S#Tx): Runner.State = internalRef().external

  def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
    if (state != Stopped) return
    val tForce    = timeRef.force
    val newState  = prepareNoFire(tForce)
    fire(newState.external)
  }

  private def prepareNoFire(timeRef: TimeRef)(implicit tx: S#Tx): InternalState[S] = {
    // prepareTimeRef() = timeRef

    // this can happen if `play` is called with a
    // different `timeRef` from what was previously prepared
    clearPlayState()

    val childViews = childAttrRef()
    val prepObs: Map[Elem, Disposable[S#Tx]] = childViews.iterator.flatMap { childView =>
      prepareChild(childView, childTime = timeRef)
    } .toMap

    val st = IPreparing(prepObs, timeRef)
    internalRef() = st
    st
  }

  private def prepareChild(childView: Elem, childTime: TimeRef)
                          (implicit tx: S#Tx): Option[(Elem, Disposable[S#Tx])] = {
    childView.prepare(childTime)
    val isPrepared = childView.state == Prepared
    if (isPrepared) None else {
      val childObs = childView.react { implicit tx => {
        case Prepared => childPreparedOrRemoved(childView)
        case _        =>
      }}
      Some(childView -> childObs) // preparingViews.put(childView, childObs)
    }
    // isPrepared
  }

  // called by `prepareChild` for each child view when it becomes ready
  private def childPreparedOrRemoved(childView: Elem)(implicit tx: S#Tx): Unit =
    internalRef() match {
      case prep: IPreparing[S] =>
        prep.map.get(childView).foreach { obs =>
          obs.dispose()
          val map1      = prep.map - childView
          val prep1     = prep.copy(map = map1)
          internalRef() = prep1
          val st = prep1.external
          if (st === Prepared) fire(Prepared)
        }
      case _ =>
    }

  def run(timeRef: TimeRef.Option, target: Target[S])(implicit tx: S#Tx): Unit = {
    val st0 = state
    if (st0 === Running) return

    val tForce  = timeRef.force
    val st1     = IPlaying(sched.time, tForce, target)
    // if (st == Stopped || prepareTimeRef() != tForce) prepareNoFire(tForce)
    internalRef.swap(st1).dispose()

    val childViews = childAttrRef()
    childViews.foreach { childView =>
      childView.run(tForce, target)
    }

    fire(Running)
  }

  def stop()(implicit tx: S#Tx): Unit = if (state != Stopped) {
    stopNoFire()
    fire(Stopped)
  }

  private def stopNoFire()(implicit tx: S#Tx): Unit = {
    clearPlayState()  // first this, so no more child observers
    val childViews = childAttrRef()
    childViews.foreach(_.stop())
  }

  @inline
  private[this] def clearPlayState()(implicit tx: S#Tx): Unit =
    internalRef.swap(_IStopped).dispose()

  def dispose()(implicit tx: S#Tx): Unit = {
    clearPlayState()
    obs.dispose()
    val childViews = childAttrRef()
    childViews.foreach(_.dispose())
  }
}
