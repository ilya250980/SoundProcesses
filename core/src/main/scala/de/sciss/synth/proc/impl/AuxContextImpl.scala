/*
 *  AuxContextImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, IdentifierMap}

import scala.language.higherKinds

/** Building block for things like AuralContext. */
trait AuxContextImpl[S <: stm.Sys[S]] {
  // ---- abstract ----

  /** Objects */
  protected def auxMap      : IdentifierMap[S#ID, S#Tx, Any]
  /** Observers */
  protected def auxObservers: IdentifierMap[S#ID, S#Tx, List[AuxObserver]]

  // ---- impl ----

  protected final class AuxObserver(idH: stm.Source[S#Tx, S#ID],
                                    val fun: S#Tx => AuxContext.Update[S, Any] => Unit)
    extends Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = {
      val id    = idH()
      val list0 = auxObservers.getOrElse(id, Nil)
      val list1 = list0.filterNot(_ == this)
      if (list1.isEmpty) auxObservers.remove(id) else auxObservers.put(id, list1)
    }
  }

  final def observeAux[A](id: S#ID)(fun: S#Tx => AuxContext.Update[S, A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val list0 = auxObservers.getOrElse(id, Nil)
    val obs   = new AuxObserver(tx.newHandle(id), fun.asInstanceOf[S#Tx => AuxContext.Update[S, Any] => Unit])
    val list1 = obs :: list0
    auxObservers.put(id, list1)
    obs
  }

  final def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit = {
    auxMap.put(id, value)
    implicit val itx = tx.peer
    val list = auxObservers.getOrElse(id, Nil)
    if (list.nonEmpty) {
      val upd = AuxContext.Added(id, value)
      list.foreach { obs =>
        obs.fun(tx)(upd)
      }
    }
  }

  final def getAux[A](id: S#ID)(implicit tx: S#Tx): Option[A] = auxMap.get(id).asInstanceOf[Option[A]]

  final def removeAux(id: S#ID)(implicit tx: S#Tx): Unit = {
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