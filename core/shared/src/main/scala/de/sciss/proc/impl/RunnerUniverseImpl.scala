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
import de.sciss.lucre.synth.{RT, Server}
import de.sciss.lucre.{Cursor, Disposable, Folder, Obj, Workspace, synth}
import de.sciss.proc.Runner.Factory
import de.sciss.proc.{Action, AuralContext, AuralSystem, Control, GenContext, Proc, Runner, Scheduler, SoundProcesses, Timeline, Universe}

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
      val res0      = new Impl[T](gen, scheduler, aural).init()
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
    new Impl[T](genContext, scheduler, auralSystem).init()
  }

  private[this] val handlerMap = TMap.empty[Workspace[_], Impl[_]]

  private final class Impl[T <: synth.Txn[T]](val genContext: GenContext[T], val scheduler: Scheduler[T],
                                         val auralSystem: AuralSystem)
                                        (implicit val cursor: Cursor[T], val workspace: Workspace[T])
    extends Universe[T] with ObservableImpl[T, Universe.Update[T]] { impl =>

    private[this] val runnersRef  = Ref(Vec.empty[Runner[T]])
    private[this] val useCount    = Ref(0)
    private[this] var obsAural: Disposable[T] = _
    private[this] val auralContextRef = Ref(Option.empty[AuralContext[T]])

    def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[T])(implicit tx: T): Universe[T] =
      new Impl[T](genContext = genContext, scheduler = newScheduler, auralSystem = newAuralSystem).init()

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

    def init()(implicit tx: T): this.type = {
      workspace.addDependent(dependent)
      obsAural = auralSystem.react { implicit tx => {
        case AuralSystem.Running(server)  => auralStarted(server)
        case AuralSystem.Stopped          => auralStopped()
        case _ =>
      }}
      auralSystem.serverOption.foreach { server =>
        auralStartedTx(server, dispatch = false)
      }
      this
    }

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

    // ---- aural ----

    override def auralContext(implicit t: T): Option[AuralContext[T]] =
      auralContextRef()

    private def auralStarted(server: Server)(implicit tx: RT): Unit = {
      // The reasoning for the txn decoupling
      // is the discrepancy between Txn and T
      tx.afterCommit {
        SoundProcesses.step[T]("auralStarted") { implicit tx: T =>
          auralStartedTx(server, dispatch = true)
        }
      }
    }

    private def auralStopped()(implicit tx: RT): Unit =
      tx.afterCommit {
        SoundProcesses.step[T]("auralStopped") { implicit tx: T =>
          auralStoppedTx()
        }
      }

    private def auralStartedTx(server: Server, dispatch: Boolean)(implicit tx: T): Unit = {
      val auralContext = AuralContext[T](server)(tx, this)
      auralContextRef() = Some(auralContext)
      if (dispatch) fire(Universe.AuralStarted[T](auralContext))
    }

    private def auralStoppedTx()(implicit tx: T): Unit = {
      auralContextRef() = None
      fire(Universe.AuralStopped())
    }
  }
}
