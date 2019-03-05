/*
 *  InMemoryLike.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import de.sciss.lucre.stm

object InMemoryLike {
  trait Txn[S <: Sys[S]] extends stm.InMemoryLike.Txn[S] with Sys.Txn[S]
}
trait InMemoryLike[S <: InMemoryLike[S]] extends stm.InMemoryLike[S] with Sys[S] {
  type Tx <: InMemoryLike.Txn[S]
}