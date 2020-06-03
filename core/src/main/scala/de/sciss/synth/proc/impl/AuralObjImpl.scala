/*
 *  AuralObjImpl.scala
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

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.Factory
import de.sciss.synth.proc.{Action, ActionRaw, AuralContext, AuralObj, Control, Ensemble, Proc, Runner, TimeRef, Timeline}

object AuralObjImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (factoryMap.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    factoryMap += tid -> f
  }

  def factories: Iterable[Factory] = factoryMap.values

  def apply[S <: Sys[S]](obj: Obj[S], attr: Runner.Attr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val tid = obj.tpe.typeId
    val opt: Option[Factory] = factoryMap.get(tid)
    opt.fold[AuralObj[S]](Generic(obj)) { f =>
      f.apply[S](obj.asInstanceOf[f.Repr[S]], attr = attr)
    }
  }

  // XXX TODO: we should have one map -- in RunnerUniverseImpl -- and give up AuralObj.Factory ?
  private var factoryMap = Map[Int, Factory](
    Action    .typeId -> AuralObj.Action,
    ActionRaw .typeId -> AuralObj.ActionRaw,
    Control   .typeId -> AuralObj.Control,
    Ensemble  .typeId -> AuralObj.Ensemble,
    Folder    .typeId -> AuralObj.Folder,
    Proc      .typeId -> AuralObj.Proc,
    Timeline  .typeId -> AuralObj.Timeline,
  )

  // -------- Generic --------

  object Generic {
    def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): AuralObj[S] =
      new Impl(tx.newHandle(obj))

    private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Obj[S]])
      extends AuralObj[S] with DummyObservableImpl[S] {

      def tpe: Obj.Type = throw new UnsupportedOperationException("Generic.tpe")

      type Repr = Obj[S]

      def obj(implicit tx: S#Tx): Obj[S] = objH()

      def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = ()

      def stop(/* time: Long */)(implicit tx: S#Tx): Unit = ()

      // def latencyEstimate(implicit tx: S#Tx): Long = 0L

      def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = () // Generic.dummyPrep

      def dispose()(implicit tx: S#Tx): Unit = ()

      def state(implicit tx: S#Tx): Runner.State = Runner.Stopped
    }
  }
}