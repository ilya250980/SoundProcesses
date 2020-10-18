/*
 *  AuralGraphemeBase.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.{BiPin, Disposable, Obj, Txn}
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.{AuralViewBase, Grapheme, Runner, TimeRef, logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralGraphemeBase {
  final case class ElemHandle[T <: Txn[T], Elem](start: Long, view: Elem)
}

/** Base for `AuralGraphemeAttribute`
  *
  * @see [[AuralGraphemeAttribute]]
  */
trait AuralGraphemeBase[T <: Txn[T], I <: Txn[I], Target, Elem <: AuralViewBase[T, Target]]
  extends AuralScheduledBase[T, Target, Elem] with ObservableImpl[T, Runner.State] {

  impl: AuralViewBase[T, Target] =>

  // ---- abstract ----

  protected def viewTree: SkipList.Map[I, Long, Vec[Elem]]

  protected def iSys: T => I

  protected def makeViewElem(start: Long, obj: Obj[T])(implicit tx: T): Elem

  // ---- impl ----

  type Repr = Grapheme[T]

  private[this] val playingRef = Ref(Option.empty[ElemHandle])

  private[this] var grObserver: Disposable[T] = _

  final def tpe: Obj.Type = Grapheme

  protected type ViewId     = Unit
  protected type ElemHandle = AuralGraphemeBase.ElemHandle[T, Elem]
  protected type Model      = Obj[T]

  protected final def viewEventAfter(offset: Long)(implicit tx: T): Long =
    viewTree.ceil(offset + 1)(iSys(tx)).fold(Long.MaxValue)(_._1)

  protected final def modelEventAfter(offset: Long)(implicit tx: T): Long =
    obj.eventAfter(offset).getOrElse(Long.MaxValue)

  protected final def processPlay(timeRef: TimeRef, target: Target)(implicit tx: T): Unit = {
    implicit val itx: I = iSys(tx)
    viewTree.floor(timeRef.offset).foreach { case (start, entries) =>
      playEntry(entries, start = start, timeRef = timeRef, target = target)
    }
  }

  protected final def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: T): Unit = {
    val start   = timeRef.offset
    val entries = viewTree.get(start)(iSys(tx))
      .getOrElse(throw new IllegalStateException(s"No element at event ${timeRef.offset}"))
    playEntry(entries, start = start, timeRef = timeRef, target = play.target)
  }

  protected final def processPrepare(spanP: Span, timeRef: TimeRef, initial: Boolean)
                                    (implicit tx: T): Iterator[PrepareResult] = {
    // println(s"processPrepare($span, $timeRef, initial = $initial")
    val gr    = obj
    val opt0  = if (initial) gr.floor(spanP.start) else gr.ceil(spanP.start)
    opt0.fold[Iterator[PrepareResult]](Iterator.empty) { e0 =>
      new Iterator[PrepareResult] {
        // updated in `advance`:
        private[this] var _child    : Obj[T]                  = _
        private[this] var _childSpan: Span.HasStart           = _
        private[this] var _ended    : Boolean                 = _
        private[this] var _succOpt  : Option[(Obj[T], Long)]  = _

        def hasNext: Boolean = !_ended

        private def advance(child: Obj[T], start: Long): Unit =
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

        def next(): (ViewId, SpanLike, Obj[T]) = {
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

  def init(gr: Grapheme[T])(implicit tx: T): this.type = {
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

  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)(implicit tx: T): Unit = {
    val view = elemFromHandle(h)
    logA(s"grapheme - playView: $view - $timeRef")
    stopViews()
    view.run(timeRef, target)
    playingRef() = Some(h)
  }

  protected def stopView(h: ElemHandle)(implicit tx: T): Unit =
    if (playingRef().contains(h))
      stopViews()

  protected def stopViews()(implicit tx: T): Unit =
    playingRef.swap(None).foreach { h =>
      val view = elemFromHandle(h)
      logA(s"aural - stopView: $view")
      view.stop()
      view.dispose()
      removeView(h)
    }

  protected def elemFromHandle(h: ElemHandle): Elem = h.view

  protected def mkView(vid: Unit, span: SpanLike, obj: Obj[T])(implicit tx: T): ElemHandle = {
    implicit val itx: I = iSys(tx)
    val Span.HasStart(start) = span
    val view  = makeViewElem(start, obj)
    val seq0  = viewTree.get(start).getOrElse(Vector.empty)
    val seq1  = seq0 :+ view
    viewTree.put(start, seq1)
    val h     = ElemHandle(start, view)
    h
  }

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: T): Boolean =
    !elemPlays && {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start == oldTarget
      h.start > currentOffset && h.start == oldTarget
    }

  override def dispose()(implicit tx: T): Unit = {
    super.dispose()
    grObserver.dispose()
  }

  // ---- private ----

  protected final def ElemHandle(start: Long, view: Elem): ElemHandle =
    AuralGraphemeBase.ElemHandle(start, view)

  private def playEntry(entries: Vec[Elem], start: Long, timeRef: TimeRef, target: Target)
                       (implicit tx: T): Unit = {
    // val start     = timeRef.offset
    val toStart   = entries.head
    val stop      = viewEventAfter(start)
    val span      = if (stop == Long.MaxValue) Span.From(start) else Span(start, stop)
    val h         = ElemHandle(start, toStart)
    val childTime = timeRef.child(span)
    playView(h, childTime, target)
  }

  private def removeView(h: ElemHandle)(implicit tx: T): Unit = {
    implicit val itx: I = iSys(tx)
    val start = h.start
    val seq0  = viewTree.get(start).get
    val idx   = seq0.indexOf(h.view)
    if (idx < 0) throw new IllegalStateException(s"View ${h.view} not found.")
    val seq1  = seq0.patch(idx, Nil, 1)
    if (seq1.isEmpty) viewTree.remove(start) else viewTree.put(start, seq1)
  }

  private def elemAdded(pin: BiPin[T, Obj[T]], start: Long, child: Obj[T])(implicit tx: T): Boolean = {
    val span = pin.eventAfter(start).fold[SpanLike](Span.From(start))(Span(start, _))
    elemAdded((), span = span, obj = child)
    val elemPlays = playingRef().exists(_.start == start)
    elemPlays
  }

  private def elemRemoved(start: Long, child: Model)(implicit tx: T): Boolean = {
    // implicit val itx = iSys(tx)
    val opt = for {
      seq  <- viewTree.get(start)(iSys(tx))
      view <- seq.find(_.obj == child)
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
  private def playingElemRemoved(pin: BiPin[T, Obj[T]], offset: Long)(implicit tx: T): Unit =
    pin.floor(offset).foreach { entry =>
      val child = entry.value
      val start = entry.key.value
      elemAdded(pin, start = start, child = child)
    }
}