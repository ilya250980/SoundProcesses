/*
 *  AuralContextImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj}
import de.sciss.lucre.synth.{Server, Sys}

import scala.concurrent.stm.Ref

object AuralContextImpl {
  def apply[S <: Sys[S]](server: Server, sched: Scheduler[S])
                        (implicit tx: S#Tx, workspaceHandle: WorkspaceHandle[S]): AuralContext[S] = {
    val objMap  = tx.newInMemoryIDMap[Entry[S]]
    val auxMap  = tx.newInMemoryIDMap[Any]
    val res     = new Impl[S](objMap, auxMap, sched, server, tx)
    logAural(s"create context ${res.hashCode().toHexString}")
    // (new Throwable).printStackTrace()
    res
  }

  private final class Entry[S <: Sys[S]](val data: Disposable[S#Tx]) {
    val count = Ref(0)
  }

  private final class Impl[S <: Sys[S]](objMap: IdentifierMap[S#ID, S#Tx, Entry[S]],
                                           protected val auxMap: IdentifierMap[S#ID, S#Tx, Any],
                                           val scheduler: Scheduler[S],
                                           val server: Server, tx0: S#Tx)
                                          (implicit val workspaceHandle: WorkspaceHandle[S])
    extends AuralContext[S] with AuxContextImpl[S] {

    protected val auxObservers: IdentifierMap[S#ID, S#Tx, List[AuxObserver]] = tx0.newInMemoryIDMap

    def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A = {
      val id = obj.id
      val e  = objMap.getOrElse(id, {
        val e0 = new Entry[S](init)
        objMap.put(id, e0)
        e0
      })
      e.count.transform(_ + 1)(tx.peer)
      e.data.asInstanceOf[A]
    }

    def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A] =
      objMap.get(obj.id).map(_.data.asInstanceOf[A])

    def release(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val id  = obj.id
      val e   = objMap.getOrElse(id, sys.error(s"No data cached for $obj"))
      val c   = e.count.transformAndGet(_ - 1)(tx.peer)
      if (c == 0) {
        objMap.remove(id)
        e.data.dispose()
      }
    }
  }
}