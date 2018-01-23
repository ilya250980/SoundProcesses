/*
 *  AuralGraphemeAttribute.scala
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

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.serial.Serializer
import de.sciss.synth.proc.AuralAttribute.{Factory, GraphemeAware, Observer}

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralGraphemeAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = Grapheme[S]

  def typeID: Int = Grapheme.typeID

  def apply[S <: Sys[S]](key: String, grapheme: Grapheme[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](key, grapheme, observer, system)
    res.init(grapheme)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](key: String, value: Grapheme[S],
                                                      observer: Observer[S], system: S { type I = I1 })
                                                     (implicit tx: S#Tx, context: AuralContext[S]): AuralGraphemeAttribute[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx _
    implicit val itx: I1#Tx = iSys(tx)
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Vec[AuralAttribute[S]]] =
        DummySerializerFactory[I1].dummySerializer

    val tree = SkipList.Map.empty[I1, Long, Vec[AuralAttribute[S]]]

    new AuralGraphemeAttribute(key, tx.newHandle(value), observer, tree /* , viewMap */)
  }
}
final class AuralGraphemeAttribute[S <: Sys[S], I <: stm.Sys[I]](val key: String,
                                                                 val obj: stm.Source[S#Tx, Grapheme[S]],
                                                                 observer: Observer[S],
                                                                 protected val viewTree: SkipList.Map[I, Long, Vec[AuralAttribute[S]]])
                                                                (implicit protected val context: AuralContext[S],
                                                                 protected val iSys: S#Tx => I#Tx)
  extends AuralGraphemeBase[S, I, AuralAttribute.Target[S], AuralAttribute[S]]
  with AuralAttribute[S]
  with Observer[S] {
  attr =>

  import TxnLike.peer

  type Elem = AuralAttribute[S]

  // we sample the first encountered objects for which temporary views
  // have to built in order to get the number-of-channels. these
  // temporary views are here and must be disposed with the parent view.
  private[this] val prefChansElemRef  = Ref[Vec[Elem]](Vector.empty)
  private[this] val prefChansNumRef   = Ref(-2)   // -2 = cache invalid. across contents of `prefChansElemRef`

  protected def makeViewElem(start: Long, child: Obj[S])(implicit tx: S#Tx): Elem = {
    val view = AuralAttribute(key, child, attr)
    view match {
      case ga: GraphemeAware[S] => ga.setGrapheme(start, obj())
      case _ =>
    }
    view
  }

  def preferredNumChannels(implicit tx: S#Tx): Int = {
    val cache = prefChansNumRef()
    if (cache > -2) {
      // println(s"preferredNumChannels - cached: $cache")
      return cache
    }

    val gr      = obj()
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
  def attrNumChannelsChanged(attr: Elem)(implicit tx: S#Tx): Unit =
    if (prefChansElemRef().contains(attr)) {  // then invalidate, otherwise ignore (what can we do?)
      prefChansNumRef() = -2
      observer.attrNumChannelsChanged(this)
    }

  override def dispose()(implicit tx: S#Tx): Unit = {
    super.dispose()
    prefChansElemRef.swap(Vector.empty).foreach(_.dispose())
  }
}