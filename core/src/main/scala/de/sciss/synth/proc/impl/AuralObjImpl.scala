/*
 *  AuralObjImpl.scala
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

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.Factory

object AuralObjImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val tid = obj.tpe.typeID
    val opt: Option[Factory] = map.get(tid)
    opt.fold[AuralObj[S]](Generic(obj)) { f =>
      f.apply[S](obj.asInstanceOf[f.Repr[S]])
    }
  }

  private var map = scala.Predef.Map[Int, Factory](
    // AudioGrapheme   .typeID -> AudioGrapheme,
    Folder          .typeID -> AuralObj.Folder,
    Proc            .typeID -> AuralObj.Proc, // AuralProcImpl
    Timeline        .typeID -> AuralObj.Timeline,
    Ensemble        .typeID -> AuralObj.Ensemble,
    Action          .typeID -> AuralObj.Action
    // Code            .typeID -> Code,
  )

  // -------- Generic --------

  object Generic {
    def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] =
      new Impl(tx.newHandle(obj))

    private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj[S]])
      extends AuralObj[S] with DummyObservableImpl[S] {

      def typeID: Int = 0

      def play(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = ()
      def stop(/* time: Long */)(implicit tx: S#Tx): Unit = ()

      // def latencyEstimate(implicit tx: S#Tx): Long = 0L

      def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = () // Generic.dummyPrep

      def dispose()(implicit tx: S#Tx): Unit = ()

      def state(implicit tx: S#Tx): AuralView.State = AuralView.Stopped
    }
  }
}