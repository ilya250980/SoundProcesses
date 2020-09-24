/*
 *  InMemory.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.synth.impl.{InMemoryImpl => Impl}

object InMemory {
  def apply(): InMemory = Impl()

  trait Txn extends InMemoryLike.Txn[InMemory.Txn] {
    type I = InMemory.Txn
  }
}

trait InMemory extends InMemoryLike[InMemory.Txn] with Sys /*[InMemory] */ {
//  type T  = InMemory.Txn
//  type I  = InMemory
}