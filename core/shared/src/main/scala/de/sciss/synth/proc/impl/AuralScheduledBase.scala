/*
 *  AuralScheduledBase.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.geom.LongPoint2D
import de.sciss.lucre.impl.{BiGroupImpl, ObservableImpl}
import de.sciss.lucre.{Disposable, Txn}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.Runner.{Prepared, Preparing, Running, Stopped}
import de.sciss.synth.proc.{AuralContext, AuralViewBase, ObjViewBase, Runner, TimeRef}
import de.sciss.synth.proc.SoundProcesses.{logAural => logA}

import scala.concurrent.stm.Ref

object AuralScheduledBase {
  final         val LOOK_AHEAD : Long = (1.0 * TimeRef.SampleRate).toLong  // one second. XXX TODO -- make configurable
  private final val PREP_FRAMES: Long = (0.5 * TimeRef.SampleRate).toLong  // XXX TODO -- make configurable
  private final val LOOK_STOP  : Long = LOOK_AHEAD + PREP_FRAMES

  private val EmptyScheduled = new Scheduled(-1, Long.MaxValue)

  final class Scheduled(val token: Int, val offset: Long) {
    override def toString = s"[token = $token, offset = ${TimeRef.framesAndSecs(offset)}]"
    def isEmpty: Boolean = token == -1
  }

  @inline
  def spanToPoint(span: SpanLike): LongPoint2D = BiGroupImpl.spanToPoint(span)
}
/** Common base for `AuralGraphemeBase` and `AuralTimelineBase`. */
trait AuralScheduledBase[T <: Txn[T], Target, Elem <: AuralViewBase[T, Target]]
  extends ObjViewBase[T, Target] with ObservableImpl[T, Runner.State] { impl =>

  // ---- abstract ----

  implicit protected val context: AuralContext[T]

  /** Called during preparation of armed elements. This
    * happens either during initial `prepare` or during grid-events.
    * Given the `prepareSpan`, the sub-class should
    *
    * - find the elements using an `intersect`
    * - for each build a view and store it somewhere
    * - for each view call `prepareChild`
    * - accumulate the results of `prepareChild` into a `Map` that is returned.
    *
    * The map will become part of `IPreparing`.
    *
    * @param initial  if `true` this is an initial preparation which means the method
    *                 must include views that start before `prepareSpan` if their span
    *                 overlaps with `prepareSpan`. If `false` this is a follow up from
    *                 `gridReached` and the search must be restricted to views that
    *                 start no earlier than `prepareSpan`.
    */
  protected def processPrepare(prepareSpan: Span, timeRef: TimeRef, initial: Boolean)
                              (implicit tx: T): Iterator[PrepareResult]

  protected type PrepareResult = (ViewId, SpanLike, Model)

  /** Called during `play`. Sub-classes should intersect
    * the current elements and for each of them call `playView`.
    */
  protected def processPlay(timeRef: TimeRef, target: Target)(implicit tx: T): Unit

  /** Called when a next interesting frame has been reached.
    * The method should look for and invoke the events such as
    * starting or stopping a view.
    */
  protected def processEvent(play: IPlaying, timeRef: TimeRef)(implicit tx: T): Unit

  /** Report the next interesting frame greater than the given frame for which
    * `eventReached` (internal) and `processEvent` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def viewEventAfter(offset: Long)(implicit tx: T): Long

  /** Report the next interesting frame greater than the given frame for which
    * `gridReached` (internal) and `processPrepare` will be called.
    * If no such event exists, the method must return `Long.MaxValue`.
    */
  protected def modelEventAfter(offset: Long)(implicit tx: T): Long

  /** An opaque type passed into `playView` that may be used by an overriding implementation.
    * Otherwise it may simply be set to `Unit`.
    */
  protected type ViewId

  /** An opaque type coming out of `processPrepare` and ending up
    * in `mkView` and `elemAdded`. Typically `Obj[T]`. */
  protected type Model

  protected type ElemHandle

  protected def elemFromHandle(h: ElemHandle): Elem

  /** Should create a new view for the given object
    * and return a handle to it. As a side effect should
    * also memorize the view in a view-tree, if such structure is maintained,
    * for later retrieval in `viewEventAfter`
    */
  protected def mkView(vid: ViewId, span: SpanLike, obj: Model)(implicit tx: T): ElemHandle

  protected def checkReschedule(h: ElemHandle, currentOffset: Long, oldTarget: Long, elemPlays: Boolean)
                               (implicit tx: T): Boolean

  /** Should be called from `processPlay`. It calls `play` on the view
    * and adds it to the list of playing views.
    * Note: `timeRef` must already have been updated through
    * appropriate intersection.
    *
    * Sub-classes may override this if they call `super.playView`
    */
  protected def playView(h: ElemHandle, timeRef: TimeRef.Option, target: Target)(implicit tx: T): Unit

  /** Should be called from `processEvent` for views that should be
    * stopped and disposed. The caller is responsible for removing
    * the view also from a view-tree if such structure is maintained.
    * NOT: This method ends by calling `viewRemoved`.
    */
  protected def stopView(h: ElemHandle)(implicit tx: T): Unit

  /** Stops and disposes all currently playing views. */
  protected def stopViews()(implicit tx: T): Unit

  // ---- impl ----

  import AuralScheduledBase.{EmptyScheduled, LOOK_AHEAD, LOOK_STOP, PREP_FRAMES, Scheduled}

  protected sealed trait InternalState extends Disposable[T] {
    def external: Runner.State
  }

  protected case object IStopped extends InternalState {
    def dispose()(implicit tx: T): Unit = ()
    def external: Runner.State = Stopped
  }

  protected sealed trait ITimedState extends InternalState {
    def timeRef: TimeRef.Option
  }

  protected final class IPreparing(val map: Map[Elem, Disposable[T]], val timeRef: TimeRef)
    extends ITimedState {

    def copy(map: Map[Elem, Disposable[T]]): IPreparing = new IPreparing(map, timeRef)

    override def toString = s"IPreparing($map, $timeRef)"

    def dispose()(implicit tx: T): Unit = map.foreach(_._2.dispose())

    def external: Runner.State = if (map.isEmpty) Prepared else Preparing
  }

  protected final class IPlaying(val wallClock: Long, val timeRef: TimeRef, val target: Target)
    extends ITimedState {

    override def toString = s"IPlaying($wallClock, $timeRef, $target)"

    def shiftTo(newWallClock: Long): TimeRef = timeRef.shift(newWallClock - wallClock)

    def dispose()(implicit tx: T): Unit = ()

    def external: Runner.State = Running
  }

//  import context.{scheduler => sched}
  import context.universe.{scheduler => sched}

  private[this] val internalRef       = Ref[InternalState](IStopped)
  private[this] val prepareSpanRef    = Ref(Span(0L, 0L))

  private[this] val schedEvtToken     = Ref(EmptyScheduled)
  private[this] val schedGridToken    = Ref(EmptyScheduled)

  final def state(implicit tx: T): Runner.State = internalRef().external

  protected final def internalState(implicit tx: T): InternalState = internalRef()
  protected final def internalState_=(value: InternalState)(implicit tx: T): Unit = internalRef() = value

  final def targetOption(implicit tx: T): Option[Target] = internalState match {
    case p: IPlaying  => Some(p.target)
    case _            => None
  }

  // ---- utility methods for sub-types ----

  /* Should be called from `processPrepare`. If the child can be instantly
   * prepared, `None` is returned. Otherwise the child is observed until it
   * is prepared and the method returns `Some(elem, observer)` that must
   * be passed back from `processPrepare` (they'll show up in `IPreparing`).
   */
  private def prepareChild(childView: Elem, childTime: TimeRef.Option, observer: Boolean)
                          (implicit tx: T): Option[(Elem, Disposable[T])] = {
    logA.debug(s"scheduled - prepare $childView - $childTime")
    childView.prepare(childTime)
    if (!observer || childView.state == Prepared) None else {
      val childObs = childView.react { implicit tx => {
        case Prepared => childPreparedOrRemoved(childView)
        case _        =>
      }}
      Some(childView -> childObs)
    }
  }

  // called by `prepareChild` for each child view when it becomes ready
  protected final def childPreparedOrRemoved(childView: Elem)(implicit tx: T): Unit =
    internalState match {
      case prep: IPreparing =>
        prep.map.get(childView).foreach { obs =>
          obs.dispose()
          val map1      = prep.map - childView
          val prep1     = prep.copy(map = map1)
          internalRef() = prep1
          val st = prep1.external
          if (st == Prepared) fire(Prepared)
        }
      case _ =>
    }

  // ----

  final def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = {
    if (state != Stopped) return
    val tForce    = timeRef.force
    val newState  = prepareNoFire(tForce)
    fire(newState.external)
  }

  /* Makes sure no more nodes are playing, then calls `processPrepare`.
   * Finally sets internal state to `IPreparing` and updates `prepareSpanRef`.
   */
  private def prepareNoFire(timeRef: TimeRef)(implicit tx: T): InternalState = {
    val startFrame    = timeRef.offset
    val stopFrame     = startFrame + LOOK_STOP
    val prepareSpan   = Span(startFrame, stopFrame)

    // this can happen if `play` is called with a
    // different `timeRef` from what was previously prepared
    /* TTT if (!tree.isEmpty(iSys(tx))) */ stopViewsAndCancelSchedule()

    val it = processPrepare(prepareSpan, timeRef, initial = true)
    val async = it.flatMap { case (vid, span, obj) =>
      val (_, prep) = mkViewAndPrepare(timeRef, vid, span, obj, observer = true)
      prep
    } .toMap

    val st            = new IPreparing(async, timeRef)
    internalRef   ()  = st
    prepareSpanRef()  = prepareSpan
    st
  }

  protected final def scheduledEvent()(implicit tx: T): Scheduled = schedEvtToken ()
  protected final def scheduledGrid ()(implicit tx: T): Scheduled = schedGridToken()

  /** Note: the prepare span will always start from current-frame and have
    * a duration of at least `LOOK_STOP`. I.e. during playback it contains the current play position.
    */
  protected final def prepareSpan()(implicit tx: T): Span = prepareSpanRef()

  /** Ensures state is consistent, then checks preparation of children.
    * If all is good, sets internal state to `IPlaying` and calls `processPlay`.
    * Next instructs scheduler and sets external state to `Playing`.
    */
  final def run(timeRef: TimeRef.Option, target: Target)(implicit tx: T): Unit = {
    val st = state
    if (st == Running) return

    val tForce    = timeRef.force
    val offset    = timeRef.offset

    if (st == Stopped || prepareSpanRef() != Span(offset, offset + LOOK_STOP)) prepareNoFire(tForce)

    val st1 = new IPlaying(sched.time, tForce, target)
    internalRef.swap(st1).dispose()

    processPlay(tForce, target)
    scheduleNextEvent(offset)
    scheduleNextGrid (offset)

    fire(Running)
  }

  /** Calls `eventAfter` to determine the next interesting frame. If that
    * frame exists, schedules the execution of `eventReached`.
    */
  private def scheduleNextEvent(currentOffset: Long)(implicit tx: T): Unit = {
    val targetOffset = viewEventAfter(currentOffset)
    val token = if (targetOffset == Long.MaxValue) -1 else {
      import TimeRef.{framesAndSecs => fas}
      logA.debug(s"scheduled - scheduleNextEvent(${fas(currentOffset)}) -> ${fas(targetOffset)}")
      val targetTime = sched.time + (targetOffset - currentOffset)
      val _token = sched.schedule(targetTime) { implicit tx =>
        eventReached(offset = targetOffset)
      }
      // logA(s"scheduled - scheduleNextEvent; token ${_token}")
      _token
    }
    val oldSched = schedEvtToken.swap(new Scheduled(token, targetOffset))
    if (oldSched.token != -1) {
      // logA(s"scheduled - scheduleNextEvent; cancel token ${oldSched.token}")
      sched.cancel(oldSched.token)
    }
  }

  /* Processes next interesting event that has been reached. If the internal
   * state is playing, calls `processEvent` followed by `scheduleNextEvent`.
   */
  private def eventReached(offset: Long)(implicit tx: T): Unit = {
    import TimeRef.{framesAndSecs => fas}
    logA.debug(s"scheduled - eventReached(${fas(offset)})")
    internalState match {
      case play: IPlaying =>
        val tr0 = play.timeRef
        val tr  = tr0.updateOffset(offset)
        processEvent(play, tr)
        scheduleNextEvent(offset)
      case _ =>
    }
  }

  /* Schedules ahead `STEP_GRID` frames to execute `gridReached`. */
  private def scheduleNextGrid(currentOffset: Long)(implicit tx: T): Unit = {
    val modelOffset = modelEventAfter(currentOffset + LOOK_STOP - 1)
    scheduleGrid(currentOffset = currentOffset, modelOffset = modelOffset)
  }

  /* `modelFrame` may be `Long.MaxValue` in which case the old schedule is simply cancelled. */
  private def scheduleGrid(currentOffset: Long, modelOffset: Long)(implicit tx: T): Unit = {
    val targetOffset  = if (modelOffset  == Long.MaxValue) Long.MaxValue else modelOffset - LOOK_AHEAD
    val token         = if (targetOffset == Long.MaxValue) -1 else {
      val targetTime = sched.time + (targetOffset - currentOffset)
      import TimeRef.{framesAndSecs => fas}
      logA.debug(s"scheduled - scheduleGrid(${fas(currentOffset)}, ${fas(modelOffset)}) -> ${fas(targetOffset)}")
      val _token = sched.schedule(targetTime) { implicit tx =>
        gridReached(offset = targetOffset)
      }
      // logA(s"scheduled - scheduleGrid; token ${_token}")
      _token
    }
    val oldSched = schedGridToken.swap(new Scheduled(token, targetOffset))
    if (oldSched.token != -1) {
      // logA(s"scheduled - scheduleGrid; cancel token ${oldSched.token}")
      sched.cancel(oldSched.token)
    }
  }

  /* If internal state is playing, calls `processPrepare` with the new prepare-span.
   * That is `LOOK_AHEAD` ahead of the `frame` we stopped at.
   */
  private def gridReached(offset: Long)(implicit tx: T): Unit = {
    import TimeRef.{framesAndSecs => fas}
    logA.debug(s"scheduled - gridReached(${fas(offset)})")
    internalState match {
      case play: IPlaying =>
        val startFrame      = offset      + LOOK_AHEAD
        val stopFrame       = startFrame  + PREP_FRAMES
        val prepareSpan     = Span(offset, stopFrame)
        prepareSpanRef()    = prepareSpan
        val searchSpan      = Span(startFrame, stopFrame)
        val timeRef         = play.shiftTo(sched.time)
        val it              = processPrepare(searchSpan, timeRef, initial = false)
        val reschedule      = it.nonEmpty

        it.foreach { case (vid, span, obj) =>
          mkViewAndPrepare(timeRef, vid, span, obj, observer = false)
        }

        // XXX TODO -- a refinement could look for eventAfter,
        // however then we need additional fiddling around in
        // `elemAdded` and `elemRemoved`...
        scheduleNextGrid(offset)
        if (reschedule) {
          val oldEvt = schedEvtToken()
          // this check addresses issue #9: when an event and grid
          // fall on the same offset, we might kill the event if it
          // wasn't executed yet. The following check ensures that
          // if there is still an event for the current offset,
          // we don't call `scheduleNextEvent`. Instead, the event
          // will be properly executed and thus issues a `scheduleNextEvent` itself
          // after running through `eventReached`
          if (oldEvt.isEmpty || oldEvt.offset != offset) {
            logA.debug("...reschedule")
            scheduleNextEvent(offset)
          }
        }

      case _ =>
    }
  }

  final def stop()(implicit tx: T): Unit = if (state != Stopped) {
    stopViewsAndCancelSchedule()
    fire(Stopped)
  }

  /** Sub-classes may override this and call `super.dispose()`
    * if they wish to free additional observers, e.g. the
    * timeline or grapheme observer.
    */
  def dispose()(implicit tx: T): Unit =
    stopViewsAndCancelSchedule()

  /* Stops playing views and disposes them. Cancels scheduled actions.
   * Then calls `clearViewsTree`. Puts internal state to `IStopped`.
   */
  private def stopViewsAndCancelSchedule()(implicit tx: T): Unit = {
    internalRef.swap(IStopped).dispose()
    stopViews()
    sched.cancel(schedEvtToken ().token)
    sched.cancel(schedGridToken().token)
  }

  // ---- helper methods for elemAdded ----

  protected final def elemAdded(vid: ViewId, span: SpanLike, obj: Model)
                               (implicit tx: T): Unit = internalState match {
      case prep: IPreparing => elemAddedPrepare(prep, vid, span, obj)
      case play: IPlaying   => elemAddedPlay   (play, vid, span, obj)
      case       IStopped   =>
    }

  protected final def elemRemoved(h: ElemHandle, elemPlays: Boolean)(implicit tx: T): Unit = {
    // val elemPlays = playingRef.contains(h)
    // remove view for the element from tree and map
    stopView(h)
    internalState match {
      case _: IPreparing =>
        val childView = elemFromHandle(h)
        childPreparedOrRemoved(childView) // might change state to `Prepared`

      case play: IPlaying =>
        // calculate current frame
        val timeRef       = play.shiftTo(sched.time)
        val currentOffset = timeRef.offset

        // re-validate the next scheduling position
        val oldSched    = scheduledEvent()
        val oldTarget   = oldSched.offset
        val reschedule  = checkReschedule(h, currentOffset = currentOffset, oldTarget = oldTarget,
                                             elemPlays = elemPlays)
        if (reschedule) {
          logA.debug("...reschedule")
          scheduleNextEvent(currentOffset)
        }

      case _ =>
    }
  }

  private def elemAddedPrepare(prep: IPreparing, vid: ViewId, span: SpanLike, obj: Model)
                              (implicit tx: T): Unit = {
    val prepS     = prepareSpan()
    if (!span.overlaps(prepS)) return

    val (childView, prepOpt) = mkViewAndPrepare(prep.timeRef, vid, span, obj, observer = true)
    prepOpt.foreach { case (_, childObs) =>
      val map1      = prep.map + (childView -> childObs)
      val prep1     = prep.copy(map = map1)
      internalState = prep1
      val st0       = prep.external
      if (st0 == Prepared) fire(Preparing)
    }
  }

  private final def mkViewAndPrepare(timeRef: TimeRef, vid: ViewId, span: SpanLike, obj: Model, observer: Boolean)
                                    (implicit tx: T): (Elem, Option[(Elem, Disposable[T])]) = {
    val childTime = timeRef.child(span)
    val h         = mkView(vid, span, obj)
    val view      = elemFromHandle(h)
    val prep      = prepareChild(view, childTime, observer = observer)
    (view, prep)
  }

  private final def elemAddedPlay(play: IPlaying, vid: ViewId, span: SpanLike, obj: Model)
                                 (implicit tx: T): Unit = {
    import de.sciss.synth.proc.Implicits.SpanComparisons

    // calculate current frame
    val timeRef       = play.shiftTo(sched.time)

    // if we're playing and the element span intersects contains
    // the current frame, play that new element
    val currentOffset = timeRef.offset
    val elemPlays     = span.contains(currentOffset)
    val elemPrepares  = !elemPlays && (span stopsAfter currentOffset) && (span startsBefore (currentOffset + LOOK_STOP))

    // re-validate the next scheduling position
    val schedEvt    = if (elemPlays) {
      // reschedule if the span has a stop and elem.stop < oldTarget
      span stopsBefore scheduledEvent().offset
    } else {
      // reschedule if the span has a start and that start is greater than the current frame,
      // and elem.start < oldTarget
      (span startsAfter currentOffset) && (span startsBefore scheduledEvent().offset)
    }

    val schedGrid = !elemPlays && !elemPrepares && (span startsAfter currentOffset) && {
      val oldGrid = scheduledGrid()
      oldGrid.isEmpty || (span startsBefore (oldGrid.offset + LOOK_AHEAD))
    }

    // react accordingly

    if (elemPlays) {
      val childTime = timeRef.child(span)
      val childView = mkView(vid, span, obj)
      playView(childView, childTime, play.target)
    }

    if (elemPrepares) {
      mkViewAndPrepare(timeRef, vid, span, obj, observer = false)
    }

    if (schedEvt) {
      logA.debug("...reschedule event")
      scheduleNextEvent(currentOffset)
    }

    if (schedGrid) {
      logA.debug("...reschedule grid")
      val Span.HasStart(modelFrame) = span
      scheduleGrid(currentOffset = currentOffset, modelOffset = modelFrame)
    }
  }
}