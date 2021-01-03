/*
 *  AuralTimelineImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc.impl

import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.expr.Context
import de.sciss.lucre.geom.{LongPoint2D, LongPoint2DLike, LongSpace, LongSquare}
import de.sciss.lucre.impl.{DummyTFormat, ObservableImpl}
import de.sciss.lucre.{BiGroup, Ident, Obj, Source, Txn, synth}
import de.sciss.serial.TFormat
import de.sciss.proc.AuralObj.Container
import de.sciss.proc.{AuralContext, AuralObj, Runner, Timeline}

object AuralTimelineImpl {
  private type Leaf[T <: Txn[T]] = AuralTimelineBase.Leaf[T, AuralObj[T]]

  import AuralTimelineBase.spanToPoint

  def apply[T <: synth.Txn[T]](timeline: Timeline[T], attr: Runner.Attr[T])
                        (implicit tx: T, context: AuralContext[T]): AuralObj.Timeline[T] = {
    val res = prepare[T, tx.I](timeline, attr)(tx, tx.inMemoryBridge, context)
    res.init(timeline)
  }

  private def prepare[T <: synth.Txn[T], I1 <: Txn[I1]](timeline: Timeline[T], attr: Context.Attr[T])
                                                 (implicit tx: T, iSys: T => I1,
                                                  context: AuralContext[T]): Impl[T, I1] = {
    implicit val itx: I1 = iSys(tx)
    implicit val pointView: (Leaf[T], I1) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeyFmt: TFormat[I1, Leaf[T]] = DummyTFormat[I1, Leaf[T]]
    import LongSpace.TwoDim

    val tree = SkipOctree.empty[I1, LongPoint2DLike, LongSquare, Leaf[T]](BiGroup.MaxSquare)

    val res = new Impl[T, I1](tx.newHandle(timeline), tree, attr)
    res
  }

  private final class Impl[T <: synth.Txn[T], I <: Txn[I]](objH: Source[T, Timeline[T]],
                                                         protected val tree: SkipOctree[I, LongPoint2DLike, LongSquare, Leaf[T]],
                                                         attr: Context.Attr[T])
                                                        (implicit protected val context: AuralContext[T],
                                                         protected val iSys: T => I)
    extends AuralTimelineBase[T, I, Unit, AuralObj[T]] with AuralObj.Timeline.Manual[T] { impl =>

    override type Repr = Timeline[T]

    protected def makeViewElem(obj: Obj[T])(implicit tx: T): AuralObj[T] = AuralObj(obj, attr)

    def obj(implicit tx: T): Timeline[T] = objH()

    object contents extends ObservableImpl[T, Container.Update[T, AuralObj.Timeline[T]]] {
      def viewAdded(id: Ident[T], view: AuralObj[T])(implicit tx: T): Unit =
        fire(Container.ViewAdded(impl, id, view))

      def viewRemoved(id: Ident[T], view: AuralObj[T])(implicit tx: T): Unit =
        fire(Container.ViewRemoved(impl, id, view))
    }

    protected def viewPlaying(h: ElemHandle)(implicit tx: T): Unit = contents.viewAdded  (h.idH(), h.view)
    protected def viewStopped(h: ElemHandle)(implicit tx: T): Unit = contents.viewRemoved(h.idH(), h.view)
  }
}
