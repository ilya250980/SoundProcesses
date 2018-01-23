/*
 *  AuxContext.scala
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

import de.sciss.lucre.stm.{Disposable, Sys}

object AuxContext {
  sealed trait Update[S <: Sys[S], +A]
  final case class Added  [S <: Sys[S], A](id: S#ID, value: A) extends Update[S, A]
  final case class Removed[S <: Sys[S]   ](id: S#ID          ) extends Update[S, Nothing]

}
trait AuxContext[S <: Sys[S]] {
  def putAux[A](id: S#ID, value: A)(implicit tx: S#Tx): Unit

  def getAux[A](id: S#ID)(implicit tx: S#Tx): Option[A]

  /** Waits for the auxiliary object to appear. If the object
    * appears the function is applied, otherwise nothing happens.
    */
  def observeAux[A](id: S#ID)(fun: S#Tx => AuxContext.Update[S, A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  def removeAux(id: S#ID)(implicit tx: S#Tx): Unit
}
