/*
 *  AuralTimelineAttribute.scala
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

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.geom.{LongPoint2D, LongPoint2DLike, LongSpace, LongSquare}
import de.sciss.lucre.impl.DummyTFormat
import de.sciss.lucre.{BiGroup, Obj, Source, Txn, synth}
import de.sciss.serial.TFormat
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, Timeline}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralTimelineAttribute extends Factory {
  import AuralTimelineBase.spanToPoint

  type Repr[T <: Txn[T]] = Timeline[T]

  private type Leaf[T <: Txn[T]] = AuralTimelineBase.Leaf[T, AuralAttribute[T]]

  def tpe: Obj.Type = Timeline

  def apply[T <: synth.Txn[T]](key: String, timeline: Timeline[T], observer: Observer[T])
                        (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] = {
    val res = prepare[T, tx.I](key, timeline, observer)(tx, tx.inMemoryBridge, context)
    res.init(timeline)
  }

  private def prepare[T <: synth.Txn[T], I1 <: Txn[I1]](key: String, value: Timeline[T],
                                                      observer: Observer[T])
                   (implicit tx: T, iSys: T => I1, context: AuralContext[T]): AuralTimelineAttribute[T, I1] = {
    implicit val itx: I1 = iSys(tx)
    implicit val pointView: (Leaf[T], I1) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeyFmt: TFormat[I1, Leaf[T]] = DummyTFormat[I1, Leaf[T]]
    import LongSpace.TwoDim

    val tree = SkipOctree.empty[I1, LongPoint2DLike, LongSquare, Leaf[T]](BiGroup.MaxSquare)

    new AuralTimelineAttribute(key, tx.newHandle(value), observer, tree)
  }
}
final class AuralTimelineAttribute[T <: synth.Txn[T],
  I <: Txn[I]](val key: String, objH: Source[T, Timeline[T]], observer: Observer[T],
               protected val tree: SkipOctree[I, LongPoint2DLike, LongSquare, AuralTimelineAttribute.Leaf[T]])
              (implicit protected val context: AuralContext[T], protected val iSys: T => I)
  extends AuralTimelineBase[T, I, AuralAttribute.Target[T], AuralAttribute[T]]
  with AuralAttribute[T]
  with Observer[T] {
  attr =>

  import Txn.peer

  type Elem = AuralAttribute[T]

  def obj(implicit tx: T): Timeline[T] = objH()

  // we sample the first encountered objects for which temporary views
  // have to built in order to get the number-of-channels. these
  // temporary views are here and must be disposed with the parent view.
  private[this] val prefChansElemRef  = Ref[Vec[Elem]](Vector.empty)
  private[this] val prefChansNumRef   = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`

  protected def makeViewElem(obj: Obj[T])(implicit tx: T): Elem = AuralAttribute(key, obj, attr)

  protected def viewPlaying(h: ElemHandle)(implicit tx: T): Unit = ()
  protected def viewStopped(h: ElemHandle)(implicit tx: T): Unit = ()

  def preferredNumChannels(implicit tx: T): Int = {
    val cache = prefChansNumRef()
    if (cache > -2) {
      // println(s"preferredNumChannels - cached: $cache")
      return cache
    }

    val timeline  = objH()
    val time0     = timeline.firstEvent.getOrElse(-1L)
    if (time0 < 0L) {
      // println(s"preferredNumChannels - empty: -1")
      return -1
    }

    val entries = timeline.intersect(time0)
    if (entries.isEmpty) {
      // println(s"preferredNumChannels - empty: -1")
      return -1
    }

    val elems = entries.flatMap(_._2.map(_.value)).toVector
    val views = elems.map(makeViewElem)
    prefChansElemRef.swap(views).foreach(_.dispose())

    @tailrec
    def loop(views: Vector[Elem], res: Int): Int = views match {
      case head +: tail =>
        val ch = head.preferredNumChannels
        // if there is any child with `-1` (undefined), we have to return
        if (ch == -1) ch else loop(tail, math.max(res, ch))
      case _ => res
    }

    val res = loop(views, -1)
    prefChansNumRef() = res
    // println(s"preferredNumChannels - ${views.size} elems yield new: $res")
    res
  }

  // if cache is affected, simply forward, so that cache is rebuilt.
  def attrNumChannelsChanged(attr: Elem)(implicit tx: T): Unit =
    if (prefChansElemRef().contains(attr)) {  // then invalidate, otherwise ignore (what can we do?)
      prefChansNumRef() = -2
      observer.attrNumChannelsChanged(this)
    }

  override def dispose()(implicit tx: T): Unit = {
    super.dispose()
    prefChansElemRef.swap(Vector.empty).foreach(_.dispose())
  }
}