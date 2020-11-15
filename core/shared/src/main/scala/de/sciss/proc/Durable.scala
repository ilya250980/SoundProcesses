/*
 *  Durable.scala
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

import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.{DataStore, DurableLike, synth}
import de.sciss.proc
import de.sciss.proc.impl.DurableImpl

object Durable {
  import proc.{Durable => S}

  def apply(factory: DataStore.Factory, mainName: String = "data"): S =
    DurableImpl(factory, mainName = mainName)

  def apply(mainStore: DataStore): S = DurableImpl(mainStore)

//  implicit def inMemory(tx: Durable#Tx): InMemory#Tx = tx.inMemory

  trait Txn extends synth.Txn[Txn] with DurableLike.Txn[Txn] {
    type I = InMemory.Txn
    def inMemory: InMemory.Txn
  }
}

trait Durable extends DurableLike[Durable.Txn] with synth.Sys {
  override def inMemory: InMemory

//  final type T  = Durable.Txn // Sys.Txn[Durable] with evt.DurableLike.Txn[Durable]
  final type I  = InMemory.Txn
}