/*
 *  AuralContextImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.{Disposable, IdentifierMap}
import de.sciss.lucre.synth.{Server, Sys}

import scala.concurrent.stm.Ref

object AuralContextImpl {
  def apply[S <: Sys[S]](server: Server, sched: Scheduler[S])(implicit tx: S#Tx): AuralContext[S] = {
    val objMap  = tx.newInMemoryIDMap[Entry[S]]
    val auxMap  = tx.newInMemoryIDMap[Any]
    new Impl[S](objMap, auxMap, sched, server)
  }

  private final class Entry[S <: Sys[S]](val data: Disposable[S#Tx]) {
    val count = Ref(0)
  }

  private final class Impl[S <: Sys[S]](objMap: IdentifierMap[S#ID, S#Tx, Entry[S]],
                                        auxMap: IdentifierMap[S#ID, S#Tx, Any],
                                        val scheduler: Scheduler[S],
                                        val server: Server)
    extends AuralContext[S] {

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

    def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit      = auxMap.put(id, value)
    def getAux[A](id: S#ID          )(implicit tx: S#Tx): Option[A] = auxMap.get(id).asInstanceOf[Option[A]]
  }
}
