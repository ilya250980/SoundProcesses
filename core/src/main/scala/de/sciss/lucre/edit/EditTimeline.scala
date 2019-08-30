/*
 *  EditTimeline.scala
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

package de.sciss.lucre.edit

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.expr.{LongObj, SpanLikeObj}
import de.sciss.lucre.stm.UndoManager.{CannotRedoException, CannotUndoException}
import de.sciss.lucre.stm.impl.BasicUndoableEdit
import de.sciss.lucre.stm.{Obj, Sys, UndoManager}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.{AudioCue, ObjKeys, Output, Proc, Timeline}

object EditTimeline {
  def add[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                      (implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      addDo  (tl, span, elem)
    ) { implicit undo =>
      addUndo(tl, span, elem)
    }

  def addUndo[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                          (implicit tx: S#Tx, undo: UndoManager[S]): Unit = {
    val edit = new Add(tl, span, elem, tx)
    undo.addEdit(edit)
  }

  def remove[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                         (implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold[Unit](
      removeDo  (tl, span, elem)
    ) { implicit undo =>
      removeUndo(tl, span, elem)
    }

  def removeUndo[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                             (implicit tx: S#Tx, undo: UndoManager[S]): Unit = {
    val edit = new Remove(tl, span, elem, tx)
    undo.addEdit(edit)
  }

  /** Try to remove links from `elem` to other processes on the timeline.
    * That has to rely on heuristics -- check global processes and objects
    * overlapping with `span` on the timeline.
    */
  def unlink[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLike, source: Output[S])
                         (implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold[Unit](
      unlinkImpl(tl, span, source)
    ) { implicit undo =>
      undo.capture("Unlink Object") {
        unlinkUndo(tl, span, source)
      }
    }

  def unlinkUndo[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLike, source: Output[S])
                             (implicit tx: S#Tx /*, undo: UndoManager[S]*/): Unit =
    unlinkImpl(tl, span, source)

  def unlinkAndRemove[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                         (implicit tx: S#Tx): Unit = {
    def removeOnly(): Unit = remove(tl, span, elem)
    elem match {
      case p: Proc[S] =>
        p.outputs.get(Proc.mainOut) match {
          case Some(out) =>
            def run(): Unit = {
              unlink(tl, span.value, out)
              removeOnly()
            }

            UndoManager.find[S].fold(run()) { undo =>
              undo.capture("Unlink Object")(run())
            }

          case None => removeOnly()
        }

      case _ => removeOnly()
    }
  }


  final case class Split[S <: Sys[S]](leftSpan  : SpanLikeObj[S], leftObj : Obj[S],
                                      rightSpan : SpanLikeObj[S], rightObj: Obj[S])

  def split[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S], time: Long)
                        (implicit tx: S#Tx): Split[S] =
    UndoManager.find[S].fold(
      splitImpl(tl, span, elem, time)
    ) { implicit undo =>
      undo.capture("Split Object") {
        splitUndo(tl, span, elem, time)
      }
    }

  def splitUndo[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S], time: Long)
                            (implicit tx: S#Tx /*, undoManager: UndoManager[S]*/): Split[S] =
    splitImpl(tl, span, elem, time)

  // ---- private: add ----

  private def addDo[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                                (implicit tx: S#Tx): Unit =
    tl.add(span, elem)

  private final class Add[S <: Sys[S]](tl0: Timeline.Modifiable[S], span0: SpanLikeObj[S], elem0: Obj[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val tlH     = tx0.newHandle(tl0)
    private[this] val spanH   = tx0.newHandle(span0)
    private[this] val elemH   = tx0.newHandle(elem0)

    addDo(tl0, span0, elem0)(tx0)

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val tl    = tlH()
      val span  = spanH()
      val elem  = elemH()
      val found = tl.remove(span, elem)
      if (!found) throw new CannotUndoException(s"$name: element was not found")
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit =
      addDo(tlH(), spanH(), elemH())

    def name: String = "Add to Timeline"
  }

  // ---- private: remove ----

  private def removeDo[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], elem: Obj[S])
                                   (implicit tx: S#Tx): Boolean =
    tl.remove(span, elem)

//  protected def any2stringadd: Any = ()

  private final class Remove[S <: Sys[S]](tl0: Timeline.Modifiable[S], span0: SpanLikeObj[S], elem0: Obj[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val tlH     = tx0.newHandle(tl0)
    private[this] val spanH   = tx0.newHandle(span0)
    private[this] val elemH   = tx0.newHandle(elem0)
    private[this] val valid   = removeDo(tl0, span0, elem0)(tx0)

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      if (!valid) return // cannotUndo()

      val tl    = tlH()
      val span  = spanH()
      val elem  = elemH()
      tl.add(span, elem)
    }

    private def invalidMessage = s"$name: element was not found"

//    private def cannotUndo(): Nothing =
//      throw new CannotUndoException(invalidMessage)

    private def cannotRedo(): Nothing =
      throw new CannotRedoException(invalidMessage)

    protected def redoImpl()(implicit tx: S#Tx): Unit = {
      val found = removeDo(tlH(), spanH(), elemH())
      if (!found) cannotRedo()
    }

    def name: String = "Remove from Timeline"
  }

  // ---- private: split ----

  // wrap this inside `undo.capture` if needed.
  private def splitImpl[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLikeObj[S], obj: Obj[S], time: Long)
                                    (implicit tx: S#Tx): Split[S] = {
    val leftObj   = obj
    val rightObj  = EditObj.copyDo[S](leftObj, connectInput = true)
    rightObj.attr.remove(ObjKeys.attrFadeIn)

    val oldVal    = span.value
    val rightSpan: SpanLikeObj.Var[S] = oldVal match {
      case Span.HasStart(leftStart) =>
        val _rightSpan = SpanLikeObj.newVar[S](oldVal)
        resizeImpl(_rightSpan, rightObj, deltaStart = time - leftStart, deltaStop = 0L, minStart = None)
        _rightSpan

      case _ =>
        val rightSpanV = oldVal.intersect(Span.from(time))
        SpanLikeObj.newVar[S](rightSpanV)
    }

    EditAttrMap.remove(leftObj.attr, ObjKeys.attrFadeOut)

    val leftSpan = span match {
      case SpanLikeObj.Var(spanVr) =>
        oldVal match {
          case Span.HasStop(rightStop) =>
            resizeImpl(spanVr, leftObj, deltaStart = 0L, deltaStop = time - rightStop, minStart = None)

          case Span.HasStart(leftStart) =>
            val leftSpanV = Span(leftStart, time)
            val _leftSpan = SpanLikeObj.newConst[S](leftSpanV)
            EditExprVar[S, SpanLike, SpanLikeObj](spanVr, _leftSpan)

          case _ =>
        }
        spanVr

      case _ =>
        EditTimeline.remove(tl, span, obj)
        val leftSpanV = oldVal.intersect(Span.until(time))
        val _leftSpan = SpanLikeObj.newVar[S](leftSpanV)
        EditTimeline.add(tl, _leftSpan, leftObj)
        _leftSpan
    }

    EditTimeline.add(tl, rightSpan, rightObj)

    // now try to find targets (tricky! we only scan global procs and their main inputs)
    (leftObj, rightObj) match {
      case (pLeft: Proc[S], pRight: Proc[S]) =>
        (pLeft.outputs.get(Proc.mainOut), pRight.outputs.get(Proc.mainOut)) match {
          case (Some(outLeft), Some(outRight)) =>
            // XXX TODO --- could add search for all objects whose span overlaps
            tl.get(Span.All).foreach { entry =>
              entry.value match {
                case sink: Proc[S] =>
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

  private def resizeImpl[S <: Sys[S]](span: SpanLikeObj.Var[S], obj: Obj[S], deltaStart: Long, deltaStop: Long,
                                      minStart: Option[Long])
                                     (implicit tx: S#Tx): Unit = {
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
      val newSpanEx = SpanLikeObj.newConst[S](newSpan)
      if (newSpanEx !== oldSpanC) {
        EditExprVar[S, SpanLike, SpanLikeObj](span, newSpanEx)
        if (dStartC != 0L) obj match {
          case objT: Proc[S] =>
            for {
              audioCue <- getAudioRegion(objT)
            } yield {
              // Crazy heuristics
              def putCue(newCue: AudioCue.Obj[S]): Unit =
                EditAttrMap.put(objT.attr, Proc.graphAudio, newCue)

              audioCue match {
                case AudioCue.Obj.Shift(peer, amt) =>
                  import de.sciss.lucre.expr.Ops.longObjOps
                  amt match {
                    case LongObj.Var(amtVr) =>
                      EditExprVar[S, Long, LongObj](amtVr, amtVr() + dStartC)
                    case _ =>
                      val newCue = AudioCue.Obj.Shift(peer, LongObj.newVar[S](amt + dStartC))
                      putCue(newCue)
                  }
                case other =>
                  val newCue = AudioCue.Obj.Shift(other, LongObj.newVar[S](dStartC))
                  putCue(newCue)
              }
            }
          case _ =>
        }
      }
    }
  }

  // ---- private: unlink ----

  private def unlinkImpl[S <: Sys[S]](tl: Timeline.Modifiable[S], span: SpanLike, source: Output[S])
                                     (implicit tx: S#Tx): Boolean = {
    val it0: Iterator[BiGroup.Entry[S, Obj[S]]] = tl.get(Span.All).iterator ++ tl.intersect(span).flatMap(_._2)
    val it = it0.collect {
      case BiGroup.Entry(_, p: Proc[S]) if EditProc.hasLink(source, p) => p
    }
    val res = it.hasNext
    it.foreach {
      EditProc.removeLink(source, _)
    }
    res
  }

//  private final class Unlink[S <: Sys[S]](tl0: Timeline.Modifiable[S], span0: SpanLikeObj[S], elem0: Obj[S], tx0: S#Tx)
//    extends BasicUndoableEdit[S] {
//  }

  // ---- aux ----

  /* Queries the audio region's grapheme segment start and audio element. */
  private def getAudioRegion[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx): Option[AudioCue.Obj[S]] =
    proc.attr.$[AudioCue.Obj](Proc.graphAudio)
}