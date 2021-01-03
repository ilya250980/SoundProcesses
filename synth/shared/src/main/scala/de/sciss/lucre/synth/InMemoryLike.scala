/*
 *  InMemoryLike.scala
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

import de.sciss.lucre.{synth, InMemoryLike => LInMemoryLike}

object InMemoryLike {
  trait Txn[T <: Txn[T]] extends LInMemoryLike.Txn[T] with synth.Txn[T]
}
trait InMemoryLike[T <: InMemoryLike.Txn[T]] extends LInMemoryLike[T] with Sys /*[T]*/ {
//  type Tx <: InMemoryLike.Txn[T]
}