/*
 *  AuralFolderAttribute.scala
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

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.{Disposable, Folder, ListObj, Obj, Source, Txn, synth}
import de.sciss.proc.AuralAttribute.{Factory, Observer, Target}
import de.sciss.proc.Runner.{Prepared, Preparing, Running, Stopped}
import de.sciss.proc.{AuralAttribute, AuralContext, Runner, TimeRef}

import scala.annotation.tailrec
import scala.concurrent.stm.Ref

object AuralFolderAttribute extends Factory {
  type Repr[T <: Txn[T]] = Folder[T]

  def tpe: Obj.Type = Folder

  def apply[T <: synth.Txn[T]](key: String, value: Folder[T], observer: Observer[T])
                              (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
    new AuralFolderAttribute(key, tx.newHandle(value), observer).init(value)


  private sealed trait InternalState[T <: Txn[T]] extends Disposable[T] {
    def external: Runner.State
  }

  private final case class IStopped[T <: Txn[T]]() extends InternalState[T] {
    def dispose()(implicit tx: T): Unit = ()
    def external: Runner.State = Stopped
  }

  private final case class IPreparing[T <: Txn[T]](map: Map[AuralAttribute[T], Disposable[T]], timeRef: TimeRef)
    extends InternalState[T] {

    def dispose()(implicit tx: T): Unit = map.foreach(_._2.dispose())

    def external: Runner.State = if (map.isEmpty) Prepared else Preparing
  }

  private final case class IPlaying[T <: Txn[T]](wallClock: Long, timeRef: TimeRef, target: Target[T])
    extends InternalState[T] {

    def shiftTo(newWallClock: Long): TimeRef = timeRef.shift(newWallClock - wallClock)

    def dispose()(implicit tx: T): Unit = ()

    def external: Runner.State = Running
  }
}
final class AuralFolderAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, Folder[T]],
                                                    observer: Observer[T])
                                                   (implicit context: AuralContext[T])
  extends AuralAttribute[T] with ObservableImpl[T, Runner.State] with AuralAttribute.Observer[T] { attr =>
//  import context.{scheduler => sched}
  import context.universe.{scheduler => sched}

  type Elem = AuralAttribute[T]

  def tpe: Obj.Type = Folder

  type Repr = Folder[T]

  import AuralFolderAttribute.{IPlaying, IPreparing, IStopped, InternalState}

  def obj(implicit tx: T): Folder[T] = objH()

  private[this] val childAttrRef  = Ref.make[Vector[Elem]]()
  private[this] val _IStopped     = IStopped[T]()
  private[this] val internalRef   = Ref[InternalState[T]](_IStopped)
  private[this] val prefChansRef  = Ref(-2)    // -2 = cache invalid

  private[this] var obs: Disposable[T] = _

  def targetOption(implicit tx: T): Option[Target[T]] = internalRef() match {
    case IPlaying(_, _, target) => Some(target)
    case _                      => None
  }

  def preferredNumChannels(implicit tx: T): Int = {
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
  def attrNumChannelsChanged(attr: Elem)(implicit tx: T): Unit =
    invalidateNumChans()

  private def invalidateNumChans()(implicit tx: T): Unit = {
    prefChansRef() = -2
    observer.attrNumChannelsChanged(this)
  }

  private def mkView(child: Obj[T])(implicit tx: T): Elem =
    AuralAttribute(key, child, attr)

  def init(folder: Folder[T])(implicit tx: T): this.type = {
    val childViews = folder.iterator.map { elem =>
      mkView(elem)
    } .toVector
    childAttrRef() = childViews

    // views.foreach(_.init())
    obs = folder.changed.react { implicit tx => upd => upd.changes.foreach {
      case ListObj.Added  (idx, child) =>
        val childView = mkView(child)
        childAttrRef.transform(_.patch(idx, childView :: Nil, 0))
        internalRef() match {
          case play: IPlaying[T] =>
            val tForce = play.shiftTo(sched.time)
            childView.run(tForce, play.target)

          case prep: IPreparing[T] =>
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

      case ListObj.Removed(idx, _ /* child */) =>
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

  def state(implicit tx: T): Runner.State = internalRef().external

  def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = {
    if (state != Stopped) return
    val tForce    = timeRef.force
    val newState  = prepareNoFire(tForce)
    fire(newState.external)
  }

  private def prepareNoFire(timeRef: TimeRef)(implicit tx: T): InternalState[T] = {
    // prepareTimeRef() = timeRef

    // this can happen if `play` is called with a
    // different `timeRef` from what was previously prepared
    clearPlayState()

    val childViews = childAttrRef()
    val prepObs: Map[Elem, Disposable[T]] = childViews.iterator.flatMap { childView =>
      prepareChild(childView, childTime = timeRef)
    } .toMap

    val st = IPreparing(prepObs, timeRef)
    internalRef() = st
    st
  }

  private def prepareChild(childView: Elem, childTime: TimeRef)
                          (implicit tx: T): Option[(Elem, Disposable[T])] = {
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
  private def childPreparedOrRemoved(childView: Elem)(implicit tx: T): Unit =
    internalRef() match {
      case prep: IPreparing[T] =>
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

  def run(timeRef: TimeRef.Option, target: Target[T])(implicit tx: T): Unit = {
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

  def stop()(implicit tx: T): Unit = if (state != Stopped) {
    stopNoFire()
    fire(Stopped)
  }

  private def stopNoFire()(implicit tx: T): Unit = {
    clearPlayState()  // first this, so no more child observers
    val childViews = childAttrRef()
    childViews.foreach(_.stop())
  }

  @inline
  private[this] def clearPlayState()(implicit tx: T): Unit =
    internalRef.swap(_IStopped).dispose()

  def dispose()(implicit tx: T): Unit = {
    clearPlayState()
    obs.dispose()
    val childViews = childAttrRef()
    childViews.foreach(_.dispose())
  }
}
