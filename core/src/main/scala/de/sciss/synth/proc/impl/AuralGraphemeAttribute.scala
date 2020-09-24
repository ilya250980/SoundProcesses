/*
 *  AuralGraphemeAttribute.scala
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

import de.sciss.lucre.{Obj, Source, Txn}
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.impl.DummyTFormat
import de.sciss.lucre.synth
import de.sciss.lucre.Txn.peer
import de.sciss.serial.TFormat
import de.sciss.synth.proc.AuralAttribute.{Factory, GraphemeAware, Observer}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, Grapheme}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralGraphemeAttribute extends Factory {
  type Repr[T <: Txn[T]] = Grapheme[T]

  def tpe: Obj.Type = Grapheme

  def apply[T <: synth.Txn[T]](key: String, grapheme: Grapheme[T], observer: Observer[T])
                        (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] = {
    val res = prepare[T, tx.I](key, grapheme, observer)(tx, tx.inMemoryBridge, context)
    res.init(grapheme)
  }

  private def prepare[T <: synth.Txn[T], I1 <: Txn[I1]](key: String, value: Grapheme[T], observer: Observer[T])
                                                       (implicit tx: T, iSys: T => I1,
                                                        context: AuralContext[T]): AuralGraphemeAttribute[T, I1] = {
    implicit val itx: I1 = iSys(tx)
    implicit val dummyKeyFmt: TFormat[I1, Vec[AuralAttribute[T]]] = DummyTFormat[I1, Vec[AuralAttribute[T]]]

    val tree = SkipList.Map.empty[I1, Long, Vec[AuralAttribute[T]]]

    new AuralGraphemeAttribute(key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralGraphemeAttribute[T <: synth.Txn[T], I <: Txn[I]](val key: String,
                                                                 objH: Source[T, Grapheme[T]],
                                                                 observer: Observer[T],
                                                                 protected val viewTree: SkipList.Map[I, Long, Vec[AuralAttribute[T]]])
                                                                (implicit protected val context: AuralContext[T],
                                                                 protected val iSys: T => I)
  extends AuralGraphemeBase[T, I, AuralAttribute.Target[T], AuralAttribute[T]]
  with AuralAttribute[T]
  with Observer[T] {
  attr =>

  type Elem = AuralAttribute[T]


  def obj(implicit tx: T): Grapheme[T] = objH()

  // we sample the first encountered objects for which temporary views
  // have to built in order to get the number-of-channels. these
  // temporary views are here and must be disposed with the parent view.
  private[this] val prefChansElemRef  = Ref[Vec[Elem]](Vector.empty)
  private[this] val prefChansNumRef   = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`

  protected def makeViewElem(start: Long, child: Obj[T])(implicit tx: T): Elem = {
    val view = AuralAttribute(key, child, attr)
    view match {
      case ga: GraphemeAware[T] => ga.setGrapheme(start, objH())
      case _ =>
    }
    view
  }

  def preferredNumChannels(implicit tx: T): Int = {
    val cache = prefChansNumRef()
    if (cache > -2) {
      // println(s"preferredNumChannels - cached: $cache")
      return cache
    }

    val gr      = objH()
    val time0   = gr.firstEvent.getOrElse(-1L)
    if (time0 < 0L) {
      // println(s"preferredNumChannels - empty: -1")
      return -1
    }

    val entries = gr.intersect(time0)
    if (entries.isEmpty) {
      // println(s"preferredNumChannels - empty: -1")
      return -1
    }

    val views = entries.map(e => makeViewElem(e.key.value, e.value))
    prefChansElemRef.swap(views).foreach(_.dispose())

    @tailrec
    def loop(views: Vec[Elem], res: Int): Int = views match {
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