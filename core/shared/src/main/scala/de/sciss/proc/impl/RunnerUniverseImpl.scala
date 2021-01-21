/*
 *  RunnerUniverseImpl.scala
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

import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.{Cursor, Disposable, Folder, Obj, Workspace, synth}
import de.sciss.proc.Runner.Factory
import de.sciss.proc.{Action, AuralSystem, Control, GenContext, Proc, Runner, Scheduler, Timeline, Universe}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TMap}

object RunnerUniverseImpl {
  private val sync = new AnyRef

  private var factoryMap = Map[Int, Factory](
    Action    .typeId -> Runner.Action,
//    ActionRaw .typeId -> Runner.ActionRaw,
    Control   .typeId -> Runner.Control,
    // Ensemble  .typeId -> Runner.Ensemble,
    Folder    .typeId -> Runner.Folder,
    Proc      .typeId -> Runner.Proc,
    Timeline  .typeId -> Runner.Timeline,
  )

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (factoryMap.contains(tid)) throw new IllegalArgumentException(s"Runner factory for type $tid already installed")
    factoryMap += tid -> f
  }

  def getFactory(tpe: Obj.Type): Option[Factory] = {
    val tid = tpe.typeId
    factoryMap.get(tid)
  }

  def factories: Iterable[Factory] = factoryMap.values

  /** Finds an existing handler for the given workspace; returns this handler or
    * creates a new one if not found.
    */
  def apply[T <: synth.Txn[T]]()(implicit tx: T, cursor: Cursor[T], workspace: Workspace[T]): Universe[T] = {
    val res = handlerMap.get(workspace).getOrElse {
      val gen       = GenContext[T]()
      val scheduler = Scheduler [T]()
      val aural     = AuralSystem(global = true)
      val res0      = new Impl[T](gen, scheduler, aural, tx)
      handlerMap.put(workspace, res0)
      res0
    }
    val resC = res.asInstanceOf[Impl[T]]
    resC.use()
    resC
  }

  /** Creates a new handler. */
  def apply[T <: synth.Txn[T]](genContext: GenContext[T], scheduler: Scheduler[T], auralSystem: AuralSystem)
                        (implicit tx: T, cursor: Cursor[T], workspace: Workspace[T]): Universe[T] = {
    new Impl[T](genContext, scheduler, auralSystem, tx)
  }

  private[this] val handlerMap = TMap.empty[Workspace[_], Impl[_]]

  private final class Impl[T <: synth.Txn[T]](val genContext: GenContext[T], val scheduler: Scheduler[T],
                                         val auralSystem: AuralSystem, tx0: T)
                                        (implicit val cursor: Cursor[T], val workspace: Workspace[T])
    extends Universe[T] with ObservableImpl[T, Universe.Update[T]] { impl =>

    private[this] val runnersRef  = Ref(Vec.empty[Runner[T]])
    private[this] val useCount    = Ref(0)

    def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[T])(implicit tx: T): Universe[T] = {
      new Impl[T](genContext = genContext, scheduler = newScheduler, auralSystem = newAuralSystem, tx0 = tx)
    }

//    def mkTransport()(implicit tx: T): Transport[T] = Transport(this)

    def runners(implicit tx: T): Iterator[Runner[T]] = runnersRef().iterator

    object dependent extends Disposable[T] {
      def dispose()(implicit tx: T): Unit = {
        handlerMap.remove(workspace)
        workspace.removeDependent(dependent)
        val r = runnersRef.swap(Vector.empty)
        r.foreach(_.dispose())
      }
    }

    workspace.addDependent(dependent)(tx0)

    def use()(implicit tx: T): Unit =
      useCount += 1

    def dispose()(implicit tx: T): Unit =
      if (useCount.transformAndGet(_ - 1) == 0) {
        dependent.dispose()
      }

    private[proc] def removeRunner(r: Runner[T])(implicit tx: T): Unit = {
      val found = runnersRef.transformAndExtract { vec0 =>
        val i       = vec0.indexOf(r)
        val _found  = i >= 0
        val vec1    = if (_found) vec0.patch(i, Nil, 1) else vec0
        (vec1, _found)
      }
      if (found) fire(Universe.Removed(r))
    }

    def mkRunner(obj: Obj[T])(implicit tx: T): Option[Runner[T]] = {
      val opt = getFactory(obj.tpe)
      opt match {
        case Some(f) =>
          val r = f.mkRunner[T](obj.asInstanceOf[f.Repr[T]])(tx, impl)
          runnersRef.transform(_ :+ r)
          fire(Universe.Added(r))
          Some(r)

        case _ => None
      }
    }
  }
}
