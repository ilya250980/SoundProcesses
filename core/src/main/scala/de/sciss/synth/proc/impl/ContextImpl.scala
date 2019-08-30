/*
 *  ContextImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm.{Disposable, IdentifierMap, Obj, Sys}

import scala.concurrent.stm.Ref

final class ContextEntry[S <: Sys[S]](val data: Disposable[S#Tx]) {
  val count: Ref[Int] = Ref(0)
}

trait ContextImpl[S <: Sys[S]] {

  protected def objMap: IdentifierMap[S#Id, S#Tx, ContextEntry[S]]

  final def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A = {
    val id = obj.id
    val e  = objMap.getOrElse(id, {
      val e0 = new ContextEntry[S](init)
      objMap.put(id, e0)
      e0
    })
    e.count.transform(_ + 1)(tx.peer)
    e.data.asInstanceOf[A]
  }

  final def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A] =
    objMap.get(obj.id).map(_.data.asInstanceOf[A])

  final def release(obj: Obj[S])(implicit tx: S#Tx): Unit = {
    val id  = obj.id
    val e   = objMap.getOrElse(id, sys.error(s"No data cached for $obj"))
    val c   = e.count.transformAndGet(_ - 1)(tx.peer)
    if (c == 0) {
      objMap.remove(id)
      e.data.dispose()
    }
  }
}