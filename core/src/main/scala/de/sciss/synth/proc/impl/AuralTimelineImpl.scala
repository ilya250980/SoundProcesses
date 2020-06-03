/*
 *  AuralTimelineImpl.scala
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

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.Context
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{DummySerializerFactory, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.serial.Serializer
import de.sciss.synth.proc.AuralObj.Container
import de.sciss.synth.proc.{AuralContext, AuralObj, Runner, Timeline}

object AuralTimelineImpl {
   private type Leaf[S <: Sys[S]] = AuralTimelineBase.Leaf[S, AuralObj[S]]

  import AuralTimelineBase.spanToPoint

  def apply[S <: Sys[S]](timeline: Timeline[S], attr: Runner.Attr[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](timeline, system, attr)
    res.init(timeline)
  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](timeline: Timeline[S], system: S { type I = I1 },
                                                      attr: Context.Attr[S])
                                                     (implicit tx: S#Tx, context: AuralContext[S]): Impl[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx
    implicit val itx: I1#Tx = iSys(tx)
    implicit val pointView: (Leaf[S], I1#Tx) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Leaf[S]] =
      DummySerializerFactory[I1].dummySerializer

    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    val res = new Impl[S, I1](tx.newHandle(timeline), tree, attr)
    res
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](objH: stm.Source[S#Tx, Timeline[S]],
                                                         protected val tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]],
                                                         attr: Context.Attr[S])
                                                        (implicit protected val context: AuralContext[S],
                                                         protected val iSys: S#Tx => I#Tx)
    extends AuralTimelineBase[S, I, Unit, AuralObj[S]] with AuralObj.Timeline.Manual[S] { impl =>

    override type Repr = Timeline[S]

    protected def makeViewElem(obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] = AuralObj(obj, attr)

    def obj(implicit tx: S#Tx): Timeline[S] = objH()

    object contents extends ObservableImpl[S, Container.Update[S, AuralObj.Timeline[S]]] {
      def viewAdded(id: S#Id, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(Container.ViewAdded(impl, id, view))

      def viewRemoved(id: S#Id, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(Container.ViewRemoved(impl, id, view))
    }

    protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit = contents.viewAdded  (h.idH(), h.view)
    protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit = contents.viewRemoved(h.idH(), h.view)
  }
}
