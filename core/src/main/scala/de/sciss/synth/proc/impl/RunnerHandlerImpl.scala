/*
 *  RunnerHandlerImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Obj, WorkspaceHandle}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Runner.{Factory, Handler}

import scala.concurrent.stm.TMap

object RunnerHandlerImpl {
  private val sync = new AnyRef

  private var factoryMap = Map[Int, Factory](
//    Folder          .typeId -> AuralObj.Folder,
//    Proc            .typeId -> AuralObj.Proc,
//    Timeline        .typeId -> AuralObj.Timeline,
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
  def apply[S <: Sys[S]]()(implicit tx: S#Tx, cursor: Cursor[S], workspace: WorkspaceHandle[S]): Handler[S] = {
    val gen       = GenContext[S]
    val scheduler = Scheduler [S]
    val aural     = AuralSystem()
    apply(gen, scheduler, aural)
  }

  /** Creates a new handler. */
  def apply[S <: Sys[S]](genContext: GenContext[S], scheduler: Scheduler[S], auralSystem: AuralSystem)
                        (implicit tx: S#Tx, cursor: Cursor[S], workspace: WorkspaceHandle[S]): Handler[S] = {
    val res = handlerMap.get(workspace).getOrElse {
      val res0 = new Impl[S](genContext, scheduler, auralSystem)
      handlerMap.put(workspace, res0)
      res0
    }
    res.asInstanceOf[Handler[S]]
  }

  private[this] val handlerMap = TMap.empty[WorkspaceHandle[_], Handler[_]]

  private final class Impl[S <: Sys[S]](val genContext: GenContext[S], val scheduler: Scheduler[S],
                                        val auralSystem: AuralSystem)
                                       (implicit val cursor: stm.Cursor[S], val workspace: WorkspaceHandle[S])
    extends Handler[S] with ObservableImpl[S, Handler.Update[S]] { h =>

    def dispose()(implicit tx: S#Tx): Unit = handlerMap.remove(workspace)

    def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]] = {
      val opt = getFactory(obj.tpe)
      opt match {
        case Some(f) =>
          val r = f.mkRunner[S](obj.asInstanceOf[f.Repr[S]], h)
          fire(Handler.Added(r))
          Some(r)

        case _ => None
      }
    }
  }
}
