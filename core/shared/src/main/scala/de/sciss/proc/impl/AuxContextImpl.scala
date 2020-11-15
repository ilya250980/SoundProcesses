/*
 *  AuxContextImpl.scala
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

import de.sciss.lucre.{Disposable, Ident, IdentMap, Source, Txn}
import de.sciss.proc.AuxContext

/** Building block for things like AuralContext. */
trait AuxContextImpl[T <: Txn[T]] {
  // ---- abstract ----

  /** Objects */
  protected def auxMap      : IdentMap[T, Any]
  /** Observers */
  protected def auxObservers: IdentMap[T, List[AuxObserver]]

  // ---- impl ----

  protected final class AuxObserver(idH: Source[T, Ident[T]],
                                    val fun: T => AuxContext.Update[T, Any] => Unit)
    extends Disposable[T] {

    def dispose()(implicit tx: T): Unit = {
      val id    = idH()
      val list0 = auxObservers.getOrElse(id, Nil)
      val list1 = list0.filterNot(_ == this)
      if (list1.isEmpty) auxObservers.remove(id) else auxObservers.put(id, list1)
    }
  }

  final def observeAux[A](id: Ident[T])(fun: T => AuxContext.Update[T, A] => Unit)(implicit tx: T): Disposable[T] = {
    val list0 = auxObservers.getOrElse(id, Nil)
    val obs   = new AuxObserver(tx.newHandle(id), fun.asInstanceOf[T => AuxContext.Update[T, Any] => Unit])
    val list1 = obs :: list0
    auxObservers.put(id, list1)
    obs
  }

  final def putAux[A](id: Ident[T], value: A)(implicit tx: T): Unit = {
    auxMap.put(id, value)
//    implicit val itx = tx.peer
    val list = auxObservers.getOrElse(id, Nil)
    if (list.nonEmpty) {
      val upd = AuxContext.Added(id, value)
      list.foreach { obs =>
        obs.fun(tx)(upd)
      }
    }
  }

  final def getAux[A](id: Ident[T])(implicit tx: T): Option[A] = auxMap.get(id).asInstanceOf[Option[A]]

  final def removeAux(id: Ident[T])(implicit tx: T): Unit = {
    val list      = auxObservers.getOrElse(id, Nil)
    val hasObs    = list.nonEmpty
    val contained = hasObs && auxMap.contains(id)
    auxMap.remove(id)
    if (contained) {
      val upd = AuxContext.Removed(id)
      list.foreach { obs =>
        obs.fun(tx)(upd)
      }
    }
  }
}