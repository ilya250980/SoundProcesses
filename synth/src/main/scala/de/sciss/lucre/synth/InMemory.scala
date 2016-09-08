/*
 *  InMemory.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.stm.InMemoryLike
import de.sciss.lucre.synth.impl.{InMemoryImpl => Impl}

object InMemory {
  def apply(): InMemory = Impl()

  trait Txn extends Sys.Txn[InMemory] with InMemoryLike.Txn[InMemory]
}

trait InMemory extends InMemoryLike[InMemory] with Sys[InMemory] {
  type Tx = InMemory.Txn
  type I  = InMemory
}