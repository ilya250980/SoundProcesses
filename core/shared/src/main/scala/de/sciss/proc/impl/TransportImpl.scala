/*
 *  TransportImpl.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.Context
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.{Ident, IdentMap, Obj, Source, synth}
import de.sciss.span.Span
import de.sciss.proc.Transport.AuralStarted
import de.sciss.proc.{AuralContext, AuralObj, Scheduler, TimeRef, Transport, Universe}
import de.sciss.proc.SoundProcesses.{logTransport => logT}

import scala.concurrent.stm.{Ref, TSet}

object TransportImpl {
  def apply[T <: synth.Txn[T]](universe: Universe[T])(implicit tx: T): Transport[T] =
    apply(universe, Context.emptyAttr[T])

  def apply[T <: synth.Txn[T]](universe: Universe[T], attr: Context.Attr[T])(implicit tx: T): Transport[T] = {
    implicit val u: Universe[T] = universe
    val res = mkTransport(attr)
    res.connectAuralSystem()
    res
  }

  def apply[T <: synth.Txn[T]](context: AuralContext[T])(implicit tx: T): Transport[T] =
    apply(context, Context.emptyAttr[T])

  def apply[T <: synth.Txn[T]](context: AuralContext[T], attr: Context.Attr[T])(implicit tx: T): Transport[T] = {
    import context.universe
    val res = mkTransport(attr)
    res.auralStartedTx()(tx, context)
    res
  }

  private def mkTransport[T <: synth.Txn[T]](attr: Context.Attr[T])(implicit tx: T, universe: Universe[T]): Impl[T] = {
    val objMap  = tx.newIdentMap[Source[T, Obj[T]]]
    val viewMap = tx.newIdentMap[AuralObj[T]]
    // (new Throwable).printStackTrace()
    new Impl(objMap, viewMap, attr)
  }

  private final class Impl[T <: synth.Txn[T]](objMap : IdentMap[T, Source[T, Obj[T]]],
                                              viewMap: IdentMap[T, AuralObj[T]], attr: Context.Attr[T])
                                       (implicit val universe: Universe[T])
    extends Transport[T] with ObservableImpl[T, Transport.Update[T]] with AuralSystemTxBridge[T] {

    def scheduler: Scheduler[T] = universe.scheduler

    private final class PlayTime(val wallClock0: Long, val pos0: Long) {
      override def toString = s"[pos0 = ${TimeRef.framesAndSecs(pos0)}, time0 = $wallClock0]"

      def isPlaying: Boolean = wallClock0 != Long.MinValue

      def play()(implicit tx: T): PlayTime =
        new PlayTime(wallClock0 = scheduler.time, pos0 = pos0)

      def currentPos(implicit tx: T): Long = if (!isPlaying) pos0 else {
        val wc1   = scheduler.time
        val delta = wc1 - wallClock0
        pos0 + delta
      }

      def stop()(implicit tx: T): PlayTime =
        new PlayTime(wallClock0 = Long.MinValue, pos0 = currentPos)
    }

    // we stupidly need these because identifier-map doesn't have an iterator
    private[this] val objSet  = TSet.empty[Source[T, Obj[T]]]
    private[this] val viewSet = TSet.empty[AuralObj[T]]

    private[this] val timeBaseRef = Ref(new PlayTime(wallClock0 = Long.MinValue, pos0 = 0L))
    private[this] val contextRef  = Ref(Option.empty[AuralContext[T]])

    def views(implicit tx: T): Set[AuralObj[T]] = viewSet.toSet

    def getView    (obj: Obj  [T])(implicit tx: T): Option[AuralObj[T]] = getViewById(obj.id)
    def getViewById(id : Ident[T])(implicit tx: T): Option[AuralObj[T]] = viewMap.get(id)

    def play()(implicit tx: T): Unit = {
      val timeBase0 = timeBaseRef()
      if (timeBase0.isPlaying) return

      val timeBase1 = timeBase0.play()
      timeBaseRef() = timeBase1
      logT.debug(s"transport - play - $timeBase1")

      playViews()
      fire(Transport.Play(this, timeBase1.pos0))
    }

    private def playViews()(implicit tx: T): Unit = {
      val tr = mkTimeRef()
      logT.debug(s"transport - playViews - $tr")
      viewSet.foreach(_.run(tr, ()))
    }

    def stop()(implicit tx: T): Unit = {
      val timeBase0 = timeBaseRef()
      if (!timeBase0.isPlaying) return

      val timeBase1 = timeBase0.stop()
      timeBaseRef() = timeBase1
      logT.debug(s"transport - stop - $timeBase1")

      stopViews()
      fire(Transport.Stop(this, timeBase1.pos0))
    }

    private def stopViews()(implicit tx: T): Unit =
      viewSet.foreach(_.stop())

    def position(implicit tx: T): Long = timeBaseRef().currentPos

    def seek(position: Long)(implicit tx: T): Unit = if (this.position != position) {
      val p = isPlaying
      if (p) stopViews()

      val timeBase1 = new PlayTime(wallClock0 = if (p) scheduler.time else Long.MinValue, pos0 = position)
      timeBaseRef() = timeBase1
      logT.debug(s"transport - seek - $timeBase1")

      if (p) playViews()
      fire(Transport.Seek(this, timeBase1.pos0, isPlaying = p))
    }

    def isPlaying(implicit tx: T): Boolean = timeBaseRef().isPlaying

    def addObject(obj: Obj[T])(implicit tx: T): Unit = {
      val id = obj.id
      if (objMap.contains(id)) throw new IllegalArgumentException(s"Object $obj was already added to transport")
      val objH = tx.newHandle(obj)
      objMap.put(id, objH)
      objSet.add(objH)
      fire(Transport.ObjectAdded(this, obj))

      contextOption.foreach { implicit context =>
        val view = mkView(obj)
        if (isPlaying) view.run(mkTimeRef(), ())
      }
    }

    def removeObject(obj: Obj[T])(implicit tx: T): Unit = {
      val id    = obj.id
      // we need objH to find the index in objSeq
      val objH  = objMap.get(id) match {
        case Some(res) => res
        case None =>
          Console.err.println(s"Warning: transport - removeObject - not found: $obj")
          return
      }
      objMap.remove(id)
      objSet.remove(objH)
      // note - if server not running, there are no views
      viewMap.get(id).foreach { view =>
        viewMap.remove(id)
        viewSet.remove(view)
        if (isPlaying) view.stop()
        fire(Transport.ViewRemoved(this, view))
      }

      fire(Transport.ObjectRemoved(this, obj))
    }

    private def mkTimeRef()(implicit tx: T) = TimeRef(Span.from(0L), offset = position)

    private def mkView(obj: Obj[T])(implicit tx: T, context: AuralContext[T]): AuralObj[T] = {
      val view = AuralObj(obj, attr)
      viewMap.put(obj.id, view)
      viewSet.add(view)
      fire(Transport.ViewAdded(this, view))
      view
    }

    override def dispose()(implicit tx: T): Unit = {
      disconnectAuralSystem()
      objMap.dispose()
      objSet.foreach { obj =>
        fire(Transport.ObjectRemoved(this, obj()))
      }
      objSet.clear()
      disposeViews()
    }

    private def disposeViews()(implicit tx: T): Unit = {
      viewMap.dispose()
      viewSet.foreach { view =>
        fire(Transport.ViewRemoved(this, view))
        view.dispose()
      }
      viewSet.clear()
    }

    // ---- aural system ----

    def contextOption(implicit tx: T): Option[AuralContext[T]] = contextRef()

    def auralStartedTx()(implicit tx: T, auralContext: AuralContext[T]): Unit = {
      logT.debug(s"transport - aural-system started")
      contextRef.set(Some(auralContext))
      fire(AuralStarted(this, auralContext))
      objSet.foreach { objH =>
        val obj = objH()
        mkView(obj)
      }
      if (isPlaying) playViews()
    }

    def auralStoppedTx()(implicit tx: T): Unit = {
      logT.debug(s"transport - aural-system stopped")
      contextRef() = None
      disposeViews()
    }
  }
}
