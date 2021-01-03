/*
 *  AuralObjImpl.scala
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

import de.sciss.lucre.impl.DummyObservableImpl
import de.sciss.lucre.{Folder, Obj, Source, Txn, synth}
import de.sciss.proc.AuralObj.Factory
import de.sciss.proc.{Action, AuralContext, AuralObj, Control, Proc, Runner, TimeRef, Timeline}

object AuralObjImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (factoryMap.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    factoryMap += tid -> f
  }

  def factories: Iterable[Factory] = factoryMap.values

  def apply[T <: synth.Txn[T]](obj: Obj[T], attr: Runner.Attr[T])(implicit tx: T, context: AuralContext[T]): AuralObj[T] = {
    val tid = obj.tpe.typeId
    val opt: Option[Factory] = factoryMap.get(tid)
    opt.fold[AuralObj[T]](Generic(obj)) { f =>
      f.apply[T](obj.asInstanceOf[f.Repr[T]], attr = attr)
    }
  }

  // XXX TODO: we should have one map -- in RunnerUniverseImpl -- and give up AuralObj.Factory ?
  private var factoryMap = Map[Int, Factory](
    Action    .typeId -> AuralObj.Action,
//    ActionRaw .typeId -> AuralObj.ActionRaw,
    Control   .typeId -> AuralObj.Control,
//    Ensemble  .typeId -> AuralObj.Ensemble,
    Folder    .typeId -> AuralObj.Folder,
    Proc      .typeId -> AuralObj.Proc,
    Timeline  .typeId -> AuralObj.Timeline,
  )

  // -------- Generic --------

  object Generic {
    def apply[T <: Txn[T]](obj: Obj[T])(implicit tx: T): AuralObj[T] =
      new Impl(tx.newHandle(obj))

    private final class Impl[T <: Txn[T]](objH: Source[T, Obj[T]])
      extends AuralObj[T] with DummyObservableImpl[T] {

      def tpe: Obj.Type = throw new UnsupportedOperationException("Generic.tpe")

      type Repr = Obj[T]

      def obj(implicit tx: T): Obj[T] = objH()

      def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: T): Unit = ()

      def stop(/* time: Long */)(implicit tx: T): Unit = ()

      // def latencyEstimate(implicit tx: T): Long = 0L

      def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = () // Generic.dummyPrep

      def dispose()(implicit tx: T): Unit = ()

      def state(implicit tx: T): Runner.State = Runner.Stopped
    }
  }
}