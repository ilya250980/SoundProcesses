/*
 *  AuralObjImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.Factory
import de.sciss.synth.proc.{Action, AuralContext, AuralObj, Ensemble, Proc, Runner, TimeRef, Timeline}

object AuralObjImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val tid = obj.tpe.typeId
    val opt: Option[Factory] = map.get(tid)
    opt.fold[AuralObj[S]](Generic(obj)) { f =>
      f.apply[S](obj.asInstanceOf[f.Repr[S]])
    }
  }

  private var map = scala.Predef.Map[Int, Factory](
    // AudioGrapheme   .typeId -> AudioGrapheme,
    Folder          .typeId -> AuralObj.Folder,
    Proc            .typeId -> AuralObj.Proc, // AuralProcImpl
    Timeline        .typeId -> AuralObj.Timeline,
    Ensemble        .typeId -> AuralObj.Ensemble,
    Action          .typeId -> AuralObj.Action
    // Code            .typeId -> Code,
  )

  // -------- Generic --------

  object Generic {
    def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] =
      new Impl(tx.newHandle(obj))

    private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Obj[S]])
      extends AuralObj[S] with DummyObservableImpl[S] {

      def tpe: Obj.Type = throw new UnsupportedOperationException("Generic.tpe")

      def run(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = ()
      def stop(/* time: Long */)(implicit tx: S#Tx): Unit = ()

      // def latencyEstimate(implicit tx: S#Tx): Long = 0L

      def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = () // Generic.dummyPrep

      def dispose()(implicit tx: S#Tx): Unit = ()

      def state(implicit tx: S#Tx): Runner.State = Runner.Stopped
    }
  }
}