/*
 *  AuxContext.scala
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

package de.sciss.proc

import de.sciss.lucre.{Disposable, Ident, Txn}

object AuxContext {
  sealed trait Update[T <: Txn[T], +A]
  final case class Added  [T <: Txn[T], A](id: Ident[T], value: A) extends Update[T, A]
  final case class Removed[T <: Txn[T]   ](id: Ident[T]          ) extends Update[T, Nothing]

}
trait AuxContext[T <: Txn[T]] {
  def putAux[A](id: Ident[T], value: A)(implicit tx: T): Unit

  def getAux[A](id: Ident[T])(implicit tx: T): Option[A]

  /** Waits for the auxiliary object to appear. If the object
    * appears the function is applied, otherwise nothing happens.
    */
  def observeAux[A](id: Ident[T])(fun: T => AuxContext.Update[T, A] => Unit)(implicit tx: T): Disposable[T]

  def removeAux(id: Ident[T])(implicit tx: T): Unit
}
