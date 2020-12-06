/*
 *  EditTimeline.scala
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

package de.sciss.lucre
package edit

import de.sciss.lucre.edit.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.edit.impl.BasicUndoableEdit
import de.sciss.span.{Span, SpanLike}
import de.sciss.proc.{AudioCue, ObjKeys, Proc, Timeline}

object EditTimeline {
  def add[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                      (implicit tx: T): Unit =
    UndoManager.find[T].fold(
      addDo  (tl, span, elem)
    ) { implicit undo =>
      addUndo(tl, span, elem)
    }

  def addUndo[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                          (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new Add(tl, span, elem, tx)
    undo.addEdit(edit)
  }

  def remove[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                         (implicit tx: T): Unit =
    UndoManager.find[T].fold[Unit] {
      removeDo(tl, span, elem)
      ()
    } { implicit undo =>
      removeUndo(tl, span, elem)
      ()
    }

  def removeUndo[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                             (implicit tx: T, undo: UndoManager[T]): Unit = {
    val edit = new Remove(tl, span, elem, tx)
    undo.addEdit(edit)
  }

  /** Try to remove links from `elem` to other processes on the timeline.
    * That has to rely on heuristics -- check global processes and objects
    * overlapping with `span` on the timeline.
    */
  def unlink[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLike, source: Proc.Output[T])
                         (implicit tx: T): Unit =
    UndoManager.find[T].fold[Unit] {
      unlinkImpl(tl, span, source)
      ()
    } { implicit undo =>
      undo.capture("Unlink Object") {
        unlinkUndo(tl, span, source)
      }
    }

  def unlinkUndo[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLike, source: Proc.Output[T])
                             (implicit tx: T): Unit = {
    unlinkImpl(tl, span, source)
    ()
  }

  def unlinkAndRemove[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                         (implicit tx: T): Unit = {
    def removeOnly(): Unit = remove(tl, span, elem)
    elem match {
      case p: Proc[T] =>
        p.outputs.get(Proc.mainOut) match {
          case Some(out) =>
            def run(): Unit = {
              unlink(tl, span.value, out)
              removeOnly()
            }

            UndoManager.find[T].fold(run()) { undo =>
              undo.capture("Unlink Object")(run())
            }

          case None => removeOnly()
        }

      case _ => removeOnly()
    }
  }


  final case class Split[T <: Txn[T]](leftSpan  : SpanLikeObj[T], leftObj : Obj[T],
                                      rightSpan : SpanLikeObj[T], rightObj: Obj[T])

  def split[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T], time: Long)
                        (implicit tx: T): Split[T] =
    UndoManager.find[T].fold(
      splitImpl(tl, span, elem, time)
    ) { implicit undo =>
      undo.capture("Split Object") {
        splitUndo(tl, span, elem, time)
      }
    }

  def splitUndo[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T], time: Long)
                            (implicit tx: T /*, undoManager: UndoManager[T]*/): Split[T] =
    splitImpl(tl, span, elem, time)

  // ---- private: add ----

  private def addDo[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                                (implicit tx: T): Unit = {
    tl.add(span, elem)
    ()
  }

  private final class Add[T <: Txn[T]](tl0: Timeline.Modifiable[T], span0: SpanLikeObj[T], elem0: Obj[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val tlH     = tx0.newHandle(tl0)
    private[this] val spanH   = tx0.newHandle(span0)
    private[this] val elemH   = tx0.newHandle(elem0)

    addDo(tl0, span0, elem0)(tx0)

    protected def undoImpl()(implicit tx: T): Unit = {
      val tl    = tlH()
      val span  = spanH()
      val elem  = elemH()
      val found = tl.remove(span, elem)
      if (!found) throw new CannotUndoException(s"$name: element was not found")
    }

    protected def redoImpl()(implicit tx: T): Unit =
      addDo(tlH(), spanH(), elemH())

    def name: String = "Add to Timeline"
  }

  // ---- private: remove ----

  private def removeDo[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], elem: Obj[T])
                                   (implicit tx: T): Boolean =
    tl.remove(span, elem)

//  protected def any2stringadd: Any = ()

  private final class Remove[T <: Txn[T]](tl0: Timeline.Modifiable[T], span0: SpanLikeObj[T], elem0: Obj[T], tx0: T)
    extends BasicUndoableEdit[T] {

    private[this] val tlH     = tx0.newHandle(tl0)
    private[this] val spanH   = tx0.newHandle(span0)
    private[this] val elemH   = tx0.newHandle(elem0)
    private[this] val valid   = removeDo(tl0, span0, elem0)(tx0)

    protected def undoImpl()(implicit tx: T): Unit = {
      if (!valid) return // cannotUndo()

      val tl    = tlH()
      val span  = spanH()
      val elem  = elemH()
      tl.add(span, elem)
      ()
    }

    private def invalidMessage = s"$name: element was not found"

//    private def cannotUndo(): Nothing =
//      throw new CannotUndoException(invalidMessage)

    private def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)

    protected def redoImpl()(implicit tx: T): Unit = {
      val found = removeDo(tlH(), spanH(), elemH())
      if (!found) cannotRedo()
    }

    def name: String = "Remove from Timeline"
  }

  // ---- private: split ----

  // wrap this inside `undo.capture` if needed.
  private def splitImpl[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLikeObj[T], obj: Obj[T], time: Long)
                                    (implicit tx: T): Split[T] = {
    val leftObj   = obj
    val rightObj  = EditObj.copyDo[T](leftObj, connectInput = true)
    rightObj.attr.remove(ObjKeys.attrFadeIn)

    val oldVal    = span.value
    val rightSpan: SpanLikeObj.Var[T] = oldVal match {
      case Span.HasStart(leftStart) =>
        val _rightSpan = SpanLikeObj.newVar[T](oldVal)
        resizeImpl(_rightSpan, rightObj, deltaStart = time - leftStart, deltaStop = 0L, minStart = None)
        _rightSpan

      case _ =>
        val rightSpanV = oldVal.intersect(Span.from(time))
        SpanLikeObj.newVar[T](rightSpanV)
    }

    EditAttrMap.remove(leftObj.attr, ObjKeys.attrFadeOut)

    val leftSpan = span match {
      case SpanLikeObj.Var(spanVr) =>
        oldVal match {
          case Span.HasStop(rightStop) =>
            resizeImpl(spanVr, leftObj, deltaStart = 0L, deltaStop = time - rightStop, minStart = None)

          case Span.HasStart(leftStart) =>
            val leftSpanV = Span(leftStart, time)
            val _leftSpan = SpanLikeObj.newConst[T](leftSpanV)
            EditExprVar[T, SpanLike, SpanLikeObj](spanVr, _leftSpan)

          case _ =>
        }
        spanVr

      case _ =>
        EditTimeline.remove(tl, span, obj)
        val leftSpanV = oldVal.intersect(Span.until(time))
        val _leftSpan = SpanLikeObj.newVar[T](leftSpanV)
        EditTimeline.add(tl, _leftSpan, leftObj)
        _leftSpan
    }

    EditTimeline.add(tl, rightSpan, rightObj)

    // now try to find targets (tricky! we only scan global procs and their main inputs)
    (leftObj, rightObj) match {
      case (pLeft: Proc[T], pRight: Proc[T]) =>
        (pLeft.outputs.get(Proc.mainOut), pRight.outputs.get(Proc.mainOut)) match {
          case (Some(outLeft), Some(outRight)) =>
            // XXX TODO --- could add search for all objects whose span overlaps
            tl.get(Span.All).foreach { entry =>
              entry.value match {
                case sink: Proc[T] =>
                  val hasLink = EditProc.hasLink(outLeft, sink)
                  if (hasLink) {
                    EditProc.addLink(outRight, sink)
                  }

                case _ =>
              }
            }

          case _ =>
        }

      case _ =>
    }

    // debugCheckConsistency(s"Split left = $leftObj, oldSpan = $oldVal; right = $rightObj, rightSpan = ${rightSpan.value}")
    Split(leftSpan, leftObj, rightSpan, rightObj)
  }

  private def resizeImpl[T <: Txn[T]](span: SpanLikeObj.Var[T], obj: Obj[T], deltaStart: Long, deltaStop: Long,
                                      minStart: Option[Long])
                                     (implicit tx: T): Unit = {
    val oldSpan   = span.value
    // val minStart  = timelineModel.bounds.start
    val dStartC   = if (deltaStart >= 0) deltaStart else oldSpan match {
      case Span.HasStart(oldStart) =>
        minStart.fold(deltaStart )(m => math.max(-(oldStart - m), deltaStart))
      case _ => 0L
    }
    val dStopC   = if (deltaStop >= 0) deltaStop else oldSpan match {
      case Span.HasStop(oldStop) =>
        minStart.fold(deltaStop  )(m => math.max(-(oldStop  - m + 32 /* MinDur */), deltaStop))
      case _ => 0L
    }

    if (dStartC != 0L || dStopC != 0L) {

      // XXX TODO -- the variable contents should ideally be looked at
      // during the edit performance

      val oldSpanC = span()
      val newSpan = oldSpanC.value match {
        case Span.From (start)  => Span.From (start + dStartC)
        case Span.Until(stop )  => Span.Until(stop  + dStopC )
        case Span(start, stop)  =>
          val newStart = start + dStartC
          Span(newStart, math.max(newStart + 32 /* MinDur */, stop + dStopC))
        case other => other
      }

      import de.sciss.equal.Implicits._
      val newSpanEx = SpanLikeObj.newConst[T](newSpan)
      if (newSpanEx !== oldSpanC) {
        EditExprVar[T, SpanLike, SpanLikeObj](span, newSpanEx)
        if (dStartC != 0L) obj match {
          case objT: Proc[T] =>
            for {
              audioCue <- getAudioRegion(objT)
            } yield {
              // Crazy heuristics
              def putCue(newCue: AudioCue.Obj[T]): Unit =
                EditAttrMap.put(objT.attr, Proc.graphAudio, newCue)

              def any2stringadd: Any = ()

              audioCue match {
                case AudioCue.Obj.Shift(peer, amt) =>
                  val dStart = LongObj.newConst[T](dStartC)
                  amt match {
                    case LongObj.Var(amtVr) =>
                      import expr.Ops._
                      EditExprVar[T, Long, LongObj](amtVr, amtVr() + dStart)
                    case _ =>
                      import expr.Ops._
                      val newCue = AudioCue.Obj.Shift(peer, LongObj.newVar[T](amt + dStart))
                      putCue(newCue)
                  }
                case other =>
                  val newCue = AudioCue.Obj.Shift(other, LongObj.newVar[T](dStartC))
                  putCue(newCue)
              }
            }
            ()
          case _ =>
        }
      }
    }
  }

  // ---- private: unlink ----

  private def unlinkImpl[T <: Txn[T]](tl: Timeline.Modifiable[T], span: SpanLike, source: Proc.Output[T])
                                     (implicit tx: T): Boolean = {
    val it0: Iterator[BiGroup.Entry[T, Obj[T]]] = tl.get(Span.All).iterator ++ tl.intersect(span).flatMap(_._2)
    val it = it0.collect {
      case BiGroup.Entry(_, p: Proc[T]) if EditProc.hasLink(source, p) => p
    }
    val res = it.hasNext
    it.foreach {
      EditProc.removeLink(source, _)
    }
    res
  }

//  private final class Unlink[T <: Txn[T]](tl0: Timeline.Modifiable[T], span0: SpanLikeObj[T], elem0: Obj[T], tx0: T)
//    extends BasicUndoableEdit[T] {
//  }

  // ---- aux ----

  /* Queries the audio region's grapheme segment start and audio element. */
  private def getAudioRegion[T <: Txn[T]](proc: Proc[T])(implicit tx: T): Option[AudioCue.Obj[T]] =
    proc.attr.$[AudioCue.Obj](Proc.graphAudio)
}