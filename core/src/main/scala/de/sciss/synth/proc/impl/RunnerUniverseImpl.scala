/*
 *  RunnerUniverseImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Workspace}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.Runner.Factory
import de.sciss.synth.proc.{Action, AuralSystem, GenContext, Proc, Runner, Scheduler, Timeline, Universe}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, TMap}

object RunnerUniverseImpl {
  private val sync = new AnyRef

  private var factoryMap = Map[Int, Factory](
//    Folder          .typeId -> AuralObj.Folder,
    Proc            .typeId -> Runner.Proc,
    Timeline        .typeId -> Runner.Timeline,
//    Ensemble        .typeId -> AuralObj.Ensemble,
    Action          .typeId -> Runner.Action
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
  def apply[S <: SSys[S]]()(implicit tx: S#Tx, cursor: Cursor[S], workspace: Workspace[S]): Universe[S] = {
    val res = handlerMap.get(workspace).getOrElse {
      val gen       = GenContext[S]
      val scheduler = Scheduler [S]
      val aural     = AuralSystem(global = true)
      val res0      = new Impl[S](gen, scheduler, aural, tx)
      handlerMap.put(workspace, res0)
      res0
    }
    val resC = res.asInstanceOf[Impl[S]]
    resC.use()
    resC
  }

  /** Creates a new handler. */
  def apply[S <: SSys[S]](genContext: GenContext[S], scheduler: Scheduler[S], auralSystem: AuralSystem)
                        (implicit tx: S#Tx, cursor: Cursor[S], workspace: Workspace[S]): Universe[S] = {
    new Impl[S](genContext, scheduler, auralSystem, tx)
  }

  private[this] val handlerMap = TMap.empty[Workspace[_], Impl[_]]

  private final class Impl[S <: SSys[S]](val genContext: GenContext[S], val scheduler: Scheduler[S],
                                         val auralSystem: AuralSystem, tx0: S#Tx)
                                        (implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends Universe[S] with ObservableImpl[S, Universe.Update[S]] { impl =>

    private[this] val runnersRef  = Ref(Vec.empty[Runner[S]])
    private[this] val useCount    = Ref(0)

    def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[S])(implicit tx: S#Tx): Universe[S] = {
      new Impl[S](genContext = genContext, scheduler = newScheduler, auralSystem = newAuralSystem, tx0 = tx)
    }

//    def mkTransport()(implicit tx: S#Tx): Transport[S] = Transport(this)

    def runners(implicit tx: S#Tx): Iterator[Runner[S]] = runnersRef().iterator

    object dependent extends Disposable[S#Tx] {
      def dispose()(implicit tx: S#Tx): Unit = {
        handlerMap.remove(workspace)
        workspace.removeDependent(dependent)
        val r = runnersRef.swap(Vector.empty)
        r.foreach(_.dispose())
      }
    }

    workspace.addDependent(dependent)(tx0)

    def use()(implicit tx: S#Tx): Unit =
      useCount += 1

    def dispose()(implicit tx: S#Tx): Unit =
      if (useCount.transformAndGet(_ - 1) == 0) {
        dependent.dispose()
      }

    private[proc] def removeRunner(r: Runner[S])(implicit tx: S#Tx): Unit = {
      val found = runnersRef.transformAndExtract { vec0 =>
        val i       = vec0.indexOf(r)
        val _found  = i >= 0
        val vec1    = if (_found) vec0.patch(i, Nil, 1) else vec0
        (vec1, _found)
      }
      if (found) fire(Universe.Removed(r))
    }

    def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]] = {
      val opt = getFactory(obj.tpe)
      opt match {
        case Some(f) =>
          val r = f.mkRunner[S](obj.asInstanceOf[f.Repr[S]])(tx, impl)
          runnersRef.transform(_ :+ r)
          fire(Universe.Added(r))
          Some(r)

        case _ => None
      }
    }
  }
}
