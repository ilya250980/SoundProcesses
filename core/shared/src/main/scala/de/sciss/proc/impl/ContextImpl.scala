/*
 *  ContextImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.{Disposable, IdentMap, Obj, Txn}

import scala.concurrent.stm.Ref

final class ContextEntry[T <: Txn[T]](val data: Disposable[T]) {
  val count: Ref[Int] = Ref(0)
}

trait ContextImpl[T <: Txn[T]] {

  protected def objMap: IdentMap[T, ContextEntry[T]]

  final def acquire[A <: Disposable[T]](obj: Obj[T])(init: => A)(implicit tx: T): A = {
    val id = obj.id
    val e  = objMap.getOrElse(id, {
      val e0 = new ContextEntry[T](init)
      objMap.put(id, e0)
      e0
    })
    e.count.transform(_ + 1)(tx.peer)
    e.data.asInstanceOf[A]
  }

  final def get[A](obj: Obj[T])(implicit tx: T): Option[A] =
    objMap.get(obj.id).map(_.data.asInstanceOf[A])

  final def release(obj: Obj[T])(implicit tx: T): Unit = {
    val id  = obj.id
    val e   = objMap.getOrElse(id, sys.error(s"No data cached for $obj"))
    val c   = e.count.transformAndGet(_ - 1)(tx.peer)
    if (c == 0) {
      objMap.remove(id)
      e.data.dispose()
    }
  }
}