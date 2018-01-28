/*
 *  AuralGraphemeBase.scala
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

import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{TxnLike, Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralGraphemeBase {
  final case class ElemHandle[S <: Sys[S], Elem](start: Long, view: Elem)
}

/** Base for `AuralGraphemeAttribute`
  *
  * @see [[AuralGraphemeAttribute]]
  */
trait AuralGraphemeBase[S <: Sys[S], I <: stm.Sys[I], Target, Elem <: AuralView[S, Target]]
  extends AuralScheduledBase[S, Target, Elem] with ObservableImpl[S, AuralView.State] { impl =>

  import TxnLike.peer

  // ---- abstract ----

  def obj: stm.Source[S#Tx, Grapheme[S]]

  protected def viewTree: SkipList.Map[I, Long, Vec[Elem]]

  protected def iSys: S#Tx => I#Tx

  protected def makeViewElem(start: Long, obj: Obj[S])(implicit tx: S#Tx): Elem

  // ---- impl ----

  private[this] val playingRef = Ref(Option.empty[ElemHandle])

  private[this] var grObserver: Disposable[S#Tx] = _

  final def typeID: Int = Grapheme.typeID

  protected type ViewID     = Unit
  protected type ElemHandle = AuralGraphemeBase.ElemHandle[S, Elem]
  protected type Model      = Obj[S]

  protected final def viewEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    viewTree.ceil(offset + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  protected final def modelEventAfter(offset: Long)(implicit tx: S#Tx): Long =
    obj().eventAfter(offset).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = iSys(tx)
    viewTree.floor(timeRef.offset).foreach { case (start, entries) =>
      playEntry(entries, start = start, timeRef = timeRef, target = target)
    }
  }

  protected final def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
    val start   = timeRef.offset
    val entries = viewTree.get(start)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.offset}"))
    playEntry(entries, start = start, timeRef = timeRef, target = play.target)
  }

  protected final def processPrepare(spanP: Span, timeRef: TimeRef, initial: Boolean)
                                    (implicit tx: S#Tx): Iterator[PrepareResult] = {
    // println(s"processPrepare($span, $timeRef, initial = $initial")
    val gr    = obj()
    val opt0  = if (initial) gr.floor(spanP.start) else gr.ceil(spanP.start)
    opt0.fold[Iterator[PrepareResult]](Iterator.empty) { e0 =>
      new Iterator[PrepareResult] {
        // updated in `advance`:
        private[this] var _child    : Obj[S]                  = _
        private[this] var _childSpan: Span.HasStart           = _
        private[this] var _ended    : Boolean                 = _
        private[this] var _succOpt  : Option[(Obj[S], Long)]  = _

        def hasNext: Boolean = !_ended

        private def advance(child: Obj[S], start: Long): Unit =
          if (start >= spanP.stop) {
            _succOpt = None
            _ended   = true
          } else {
            _child = child
            gr.ceil(start + 1) match {
              case Some(succ) =>
                val stop      = succ.key.value
                _childSpan    = Span(start, stop)
                val childTime = timeRef.child(_childSpan)
                _ended        = childTime.hasEnded // .span.isEmpty
                _succOpt      = if (_ended) None else Some((succ.value, stop))

              case None =>
                _childSpan    = Span.from(start)
                val childTime = timeRef.child(_childSpan)
                _ended        = childTime.hasEnded // span.isEmpty
                _succOpt      = None
            }
          }

        advance(e0.value, e0.key.value)

        def next(): (ViewID, SpanLike, Obj[S]) = {
          if (_ended) throw new NoSuchElementException("next on empty iterator")

          val res = ((), _childSpan, _child)
          _succOpt.fold[Unit] { _ended = true } { case (succ, stop) =>
            advance(succ, stop)
          }
          res
        }
      }
    }
  }

  def init(gr: Grapheme[S])(implicit tx: S#Tx): this.type = {
    grObserver = gr.changed.react { implicit tx => upd =>
      upd.changes.foreach {
        case Grapheme.Added  (time, entry)    =>
          elemAdded  (upd.pin, time, entry.value)
        case Grapheme.Removed(time, entry)    =>
          val wasPlaying = elemRemoved(time, entry.value)
          if (wasPlaying) {
            playingElemRemoved(upd.pin, time)
          }

        case Grapheme.Moved(timeCh, entry)  =>
          // for simplicity just remove and re-add
          // ; in the future this could be optimized
          // (e.g., not deleting and re-creating the AuralView)
          val wasPlaying = elemRemoved(         timeCh.before, entry.value)
          val isPlaying  = elemAdded  (upd.pin, timeCh.now   , entry.value)
          if (wasPlaying && !isPlaying) {
            playingElemRemoved(upd.pin, timeCh.before)
          }
      }
    }
    this
  }

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)(implicit tx: S#Tx): Unit = {
    val view = elemFromHandle(h)
    logA(s"grapheme - playView: $view - $timeRef")
    stopViews()
    view.play(timeRef, target)
    playingRef() = Some(h)
  }

  protected def stopView(h: ElemHandle)(implicit tx: S#Tx): Unit =
    if (playingRef().contains(h))
      stopViews()

  protected def stopViews()(implicit tx: S#Tx): Unit =
    playingRef.swap(None).foreach { h =>
      val view = elemFromHandle(h)
      logA(s"aural - stopView: $view")
      view.stop()
      view.dispose()
      removeView(h)
    }

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  protected def mkView(vid: Unit, span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): ElemHandle = {
    implicit val itx: I#Tx = iSys(tx)
    val Span.HasStart(start) = span
    val view  = makeViewElem(start, obj)
    val seq0  = viewTree.get(start).getOrElse(Vector.empty)
    val seq1  = seq0 :+ view
    viewTree.add(start -> seq1)
    val h     = ElemHandle(start, view)
    h
  }

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: S#Tx): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.start > currentOffset && h.start == oldTarget
    }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    grObserver.dispose()
  }

  // ---- private ----

  protected final def ElemHandle(start: Long, view: Elem): ElemHandle =
    AuralGraphemeBase.ElemHandle(start, view)

  private def playEntry(entries: Vec[Elem], start: Long, timeRef: TimeRef, target: Target)
                       (implicit tx: S#Tx): Unit = {
    // val start     = timeRef.offset
    val toStart   = entries.head
    val stop      = viewEventAfter(start)
    val span      = if (stop == Long.MaxValue) Span.From(start) else Span(start, stop)
    val h         = ElemHandle(start, toStart)
    val childTime = timeRef.child(span)
    playView(h, childTime, target)
  }

  private def removeView(h: ElemHandle)(implicit tx: S#Tx): Unit = {
    implicit val itx: I#Tx = iSys(tx)
    val start = h.start
    val seq0  = viewTree.get(start).get
    val idx   = seq0.indexOf(h.view)
    if (idx < 0) throw new IllegalStateException(s"View ${h.view} not found.")
    val seq1  = seq0.patch(idx, Nil, 1)
    if (seq1.isEmpty) viewTree.remove(start) else viewTree.add(start -> seq1)
  }

  private def elemAdded(pin: BiPin[S, Obj[S]], start: Long, child: Obj[S])(implicit tx: S#Tx): Boolean = {
    val span = pin.eventAfter(start).fold[SpanLike](Span.From(start))(Span(start, _))
    elemAdded((), span = span, obj = child)
    val elemPlays = playingRef().exists(_.start == start)
    elemPlays
  }

  private def elemRemoved(start: Long, child: Model)(implicit tx: S#Tx): Boolean = {
    // implicit val itx = iSys(tx)
    val opt = for {
      seq  <- viewTree.get(start)(iSys(tx))
      view <- seq.find(_.obj() == child)
    } yield {
      logA(s"timeline - elemRemoved($start, $child)")
      val h         = ElemHandle(start, view)
      val elemPlays = playingRef().contains(h)
      elemRemoved(h, elemPlays = elemPlays)
      elemPlays
    }
    opt.contains(true)
  }

  // If a playing element has been removed, check if there is another one
  // 'below' it now. If so, create a view for it.
  private def playingElemRemoved(pin: BiPin[S, Obj[S]], offset: Long)(implicit tx: S#Tx): Unit =
    pin.floor(offset).foreach { entry =>
      val child = entry.value
      val start = entry.key.value
      elemAdded(pin, start = start, child = child)
    }
}