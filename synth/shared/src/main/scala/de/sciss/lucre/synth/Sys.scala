/*
 *  Sys.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.{Sys => LSys}

/*
object Sys {
  trait Txn[T <: Txn[T]] extends LTxn[T] with synth.Txn
}
*/

trait Sys /*[S <: Sys[S]]*/ extends LSys /*[S]*/ {
  type T <: Txn[T]
//  type I <: InMemoryLike[I]
}

//trait NoSys extends Sys[NoSys]