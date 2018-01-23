/*
 *  AuralTimelineImpl.scala
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

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.data.SkipOctree
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.geom.{LongPoint2D, LongSpace}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.serial.Serializer
import de.sciss.synth.proc.AuralObj.Container

object AuralTimelineImpl {
   private type Leaf[S <: Sys[S]] = AuralTimelineBase.Leaf[S, AuralObj[S]]

  import AuralTimelineBase.spanToPoint

  def apply[S <: Sys[S]](timeline: Timeline[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val system  = tx.system
    val res     = prepare[S, system.I](timeline, system)
    res.init(timeline)
  }

//  /** An empty view that does not listen for events on the timeline. */
//  def empty[S <: Sys[S]](timeline: Timeline[S])
//                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline.Manual[S] = {
//    println("WARNING: AuralTimelineImpl.empty -- doesn't make any sense; aural views are constructed nevertheless")
//    val system = tx.system
//    val res = prepare[S, system.I](timeline, system)
//    res.init(null)
//  }

  private def prepare[S <: Sys[S], I1 <: stm.Sys[I1]](timeline: Timeline[S], system: S { type I = I1 })
                                                     (implicit tx: S#Tx, context: AuralContext[S]): Impl[S, I1] = {
    implicit val iSys: S#Tx => I1#Tx = system.inMemoryTx _
    implicit val itx: I1#Tx = iSys(tx)
    implicit val pointView: (Leaf[S], I1#Tx) => LongPoint2D = (l, _) => spanToPoint(l._1)
    implicit val dummyKeySer: Serializer[I1#Tx, I1#Acc, Leaf[S]] =
      DummySerializerFactory[I1].dummySerializer

    val tree = SkipOctree.empty[I1, LongSpace.TwoDim, Leaf[S]](BiGroup.MaxSquare)

    val res = new Impl[S, I1](tx.newHandle(timeline), tree)
    res
  }

  private final class Impl[S <: Sys[S], I <: stm.Sys[I]](val obj: stm.Source[S#Tx, Timeline[S]],
                                                         protected val tree: SkipOctree[I, LongSpace.TwoDim, Leaf[S]])
                                                        (implicit protected val context: AuralContext[S],
                                                         protected val iSys: S#Tx => I#Tx)
    extends AuralTimelineBase[S, I, Unit, AuralObj[S]] with AuralObj.Timeline.Manual[S] { impl =>

    protected def makeViewElem(obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] = AuralObj(obj)

    object contents extends ObservableImpl[S, Container.Update[S, AuralObj.Timeline[S]]] {
      def viewAdded(id: S#ID, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(Container.ViewAdded(impl, id, view))

      def viewRemoved(id: S#ID, view: AuralObj[S])(implicit tx: S#Tx): Unit =
        fire(Container.ViewRemoved(impl, id, view))
    }

    protected def viewPlaying(h: ElemHandle)(implicit tx: S#Tx): Unit = contents.viewAdded  (h.idH(), h.view)
    protected def viewStopped(h: ElemHandle)(implicit tx: S#Tx): Unit = contents.viewRemoved(h.idH(), h.view)
  }
}
