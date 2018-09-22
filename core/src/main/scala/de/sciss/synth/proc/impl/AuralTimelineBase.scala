/*
 *  AuralTimelineBase.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.geom.{LongPoint2D, LongRectangle, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec, Set => ISet}
import scala.concurrent.stm.TSet

object AuralTimelineBase {
  type Leaf[S <: Sys[S], Elem] = (SpanLike, Vec[(stm.Source[S#Tx, S#Id], Elem)])

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)

  protected final case class ElemHandle[S <: Sys[S], Elem](idH: stm.Source[S#Tx, S#Id], span: SpanLike, view: Elem)
}
trait AuralTimelineBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: ViewBase[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, Runner.State] { impl =>

  import AuralTimelineBase.spanToPoint
  import TxnLike.peer

  // ---- abstract ----

  override def objH: stm.Source[S#Tx, Timeline[S]]

  protected def tree: SkipOctree[I, LongSpace.TwoDim, (SpanLike, Vec[(stm.Source[S#Tx, S#Id], Elem)])]

  protected def iSys: S#Tx => I#Tx

  protected def makeViewElem(obj: Obj[S])(implicit tx: S#Tx): Elem

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewAdded`.
    */
  protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit

  /** A notification method that may be used to `fire` an event
    * such as `AuralObj.Timeline.ViewRemoved`.
    */
  protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit

  // ---- impl ----

  private[this] val playingRef        = TSet.empty[ElemHandle]

  private[this] var viewMap   : IdentifierMap[S#Id, S#Tx, ElemHandle] = _
  private[this] var tlObserver: Disposable[S#Tx] = _

  protected type ElemHandle = AuralTimelineBase.ElemHandle[S, Elem]
  protected type ViewId     = S#Id
  protected type Model      = Obj[S]

  private def ElemHandle(idH: stm.Source[S#Tx, S#Id], span: SpanLike, view: Elem): ElemHandle =
    AuralTimelineBase.ElemHandle(idH, span, view)

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  final def views(implicit tx: S#Tx): ISet[Elem] = playingRef.map(elemFromHandle)(breakOut) // toSet

  final def tpe: Obj.Type = Timeline

  private type Leaf = (SpanLike, Vec[(stm.Source[S#Tx, S#Id], Elem)])

  protected final def viewEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    BiGroupImpl.eventAfter(tree)(offset)(iSys(tx)).getOrElse(Long.MaxValue)

  protected final def modelEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    objH().eventAfter(offset).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    val toStart = intersect(timeRef.offset)
    playViews(toStart, timeRef, target)
  }

  @inline
  private[this] def intersect(offset: Long)(implicit tx: S#Tx): Iterator[Leaf] =
    BiGroupImpl.intersectTime(tree)(offset)(iSys(tx))

  protected final def processPrepare(span: Span, timeRef: TimeRef, initial: Boolean)
                                    (implicit tx: S#Tx): Iterator[PrepareResult] = {
    val tl          = objH()
    // search for new regions starting within the look-ahead period
    val startSpan   = if (initial) Span.until(span.stop) else span
    val stopSpan    = Span.from(span.start)
    val it          = tl.rangeSearch(start = startSpan, stop = stopSpan)
    it.flatMap { case (childSpan, elems) =>
      val childTime = timeRef.child(childSpan)
      val sub: Vec[(ViewId, SpanLike, Obj[S])] = if (childTime.hasEnded /* span.isEmpty */) Vector.empty else {
        elems.map { timed =>
          (timed.id, childSpan, timed.value)
        }
      }
      sub
    }
  }

  protected final def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)
                              (implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    logA(s"timeline - playView: $view - $timeRef")
    view.run(timeRef, target)
    playingRef.add(h)
    viewPlaying(h)
  }

  protected final def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    logA(s"scheduled - stopView: $view")
    view.stop()
    viewStopped(h)
    view.dispose()
    playingRef.remove(h)
    removeView(h)
  }

  protected final def stopViews()(implicit tx: S#Tx): Unit =
    playingRef.foreach { view =>
      stopView(view)
    }

  protected final def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
//    val (toStartI, toStopI) = eventsAt(timeRef.offset)

    val itx       = iSys(tx)
    val stopShape = LongRectangle(BiGroup.MinCoordinate, timeRef.offset, BiGroup.MaxSide, 1)
    val toStop    = tree.rangeQuery(stopShape )(itx)

    // this is a pretty tricky decision...
    // do we first free the stopped views and then launch the started ones?
    // or vice versa?
    //
    // we stick now to stop-then-start because it seems advantageous
    // for aural-attr-target as we don't build up unnecessary temporary
    // attr-set/attr-map synths. however, I'm not sure this doesn't
    // cause a problem where the stop action schedules on immediate
    // bundle and the start action requires a sync'ed bundle? or is
    // this currently prevented automatically? we might have to
    // reverse this decision.

    // N.B.: as crucial is to understand that the iterators from `rangeQuery` may become
    // invalid if modifying the tree while iterating. Thus, we first create only the
    // `toStop` iterator, and if i not empty, force it to a stable collection. Only after
    // stopping the views, we create the `toStart` iterator which might otherwise have
    // become invalid as well. (Mellite bug #71).

    //        playViews(toStart, tr, play.target)
    //        stopAndDisposeViews(toStop)

    if (toStop.hasNext) {
      // N.B. `toList` to avoid iterator invalidation
      toStop.toList.foreach { case (span, views) =>
        views.foreach { case (idH, view) =>
          stopView(ElemHandle(idH, span, view))
        }
      }
    }

    val startShape  = LongRectangle(timeRef.offset, BiGroup.MinCoordinate, 1, BiGroup.MaxSide)
    val toStart     = tree.rangeQuery(startShape)(itx)

    playViews(toStart, timeRef, play.target)
  }

  private def playViews(it: Iterator[Leaf], timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit =
    if (it.hasNext) it.foreach { case (span, views) =>
      val tr = timeRef.child(span)
      views.foreach { case (idH, elem) =>
        playView(ElemHandle(idH, span, elem), tr, target)
      }
    }

//  // this can be easily implemented with two rectangular range searches
//  // return: (things-that-start, things-that-stop)
//  @inline
//  private[this] def eventsAt(offset: Long)(implicit tx: S#Tx): (Iterator[Leaf], Iterator[Leaf]) =
//    BiGroupImpl.eventsAt(tree)(offset)(iSys(tx))

  /** Initializes the object.
    *
    * @param tl the timeline to listen to. If `null` (yes, ugly), requires
    *           manual additional of views
    */
  def init(tl: Timeline[S])(implicit tx: S#Tx): this.type = {
    viewMap = tx.newInMemoryIdMap[ElemHandle]
    if (tl != null) tlObserver = tl.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Timeline.Added  (span, timed)    => elemAdded  (timed.id, span, timed.value)
        case Timeline.Removed(span, timed)    => elemRemoved(timed.id, span, timed.value)
        case Timeline.Moved  (spanCh, timed)  =>
          // for simplicity just remove and re-add
          // ; in the future this could be optimized
          // (e.g., not deleting and re-creating the AuralObj)
          elemRemoved(timed.id, spanCh.before, timed.value)
          elemAdded  (timed.id, spanCh.now   , timed.value)
      }
    }
    this
  }

  final def getView(timed: Timeline.Timed[S])(implicit tx: S#Tx): Option[Elem] =
    getViewById(timed.id)

  final def getViewById(id: S#Id)(implicit tx: S#Tx): Option[Elem] =
    viewMap.get(id).map(_.view)

  final def addObject(id: S#Id, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit =
    elemAdded(id, span.value, obj)

  final def removeObject(id: S#Id, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): Unit =
    elemRemoved(id, span.value, obj)

  protected final def mkView(tid: S#Id, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): ElemHandle = {
    logA(s"timeline - elemAdded($span, $obj)")

    // create a view for the element and add it to the tree and map
    val childView = makeViewElem(obj) // AuralObj(obj)
    val idH       = tx.newHandle(tid)
    val h         = ElemHandle(idH, span, childView)
    viewMap.put(tid, h)
    tree.transformAt(spanToPoint(span)) { opt =>
      // import expr.IdentifierSerializer
      val tup       = (idH, childView)
      val newViews  = opt.fold(span -> Vec(tup)) { case (span1, views) => (span1, views :+ tup) }
      Some(newViews)
    } (iSys(tx))

    // elemAddedPreparePlay(st, tid, span, childView)
    h
  }

  private def elemRemoved(tid: S#Id, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit =
    viewMap.get(tid).foreach { h =>
      // finding the object in the view-map implies that it
      // is currently preparing or playing
      logA(s"timeline - elemRemoved($span, $obj)")
      val elemPlays = playingRef.contains(h)
      elemRemoved(h, elemPlays = elemPlays)
    }

  protected final def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                                     (implicit tx: S#Tx): Boolean =
    if (elemPlays) {
      // reschedule if the span has a stop and elem.stop == oldTarget
      h.span match {
        case hs: Span.HasStop => hs.stop == oldTarget
        case _ => false
      }
    } else {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.span match {
        case hs: Span.HasStart => hs.start > currentOffset && hs.start == oldTarget
        case _ => false
      }
    }

  private def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    import h._
    logA(s"timeline - removeView - $span - $view")

    // note: this doesn't have to check for `IPreparing`, as it is called only
    // via `eventReached`, thus during playing. correct?

    // preparingViews.remove(view).foreach(_.dispose())
    tree.transformAt(spanToPoint(span)) { opt =>
      opt.flatMap { case (span1, views) =>
        val i = views.indexWhere(_._2 == view)
        val views1 = if (i >= 0) {
          views.patch(i, Nil, 1)
        } else {
          Console.err.println(s"Warning: timeline - removeView - view for $objH not in tree")
          views
        }
        if (views1.isEmpty) None else Some(span1 -> views1)
      }
    } (iSys(tx))

    viewMap.remove(idH())
  }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    // this may be the case for `AuralTimelineImpl.empty` where `init` is not called.
    if (tlObserver != null) tlObserver.dispose()
  }
}