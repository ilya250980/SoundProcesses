/*
 *  Confluent.scala
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
import de.sciss.lucre.{ConfluentLike, DataStore, confluent, synth, Durable => LDurable, InMemory => LInMemory}
import de.sciss.proc
import de.sciss.proc.impl.ConfluentImpl

object Confluent {
  import proc.{Confluent => S}

  def apply(storeFactory: DataStore.Factory): S = ConfluentImpl(storeFactory)

  trait Txn extends confluent.Txn[Txn] with synth.Txn[Txn] {
//    private[proc] def durable : Durable#Tx
//    private[proc] def inMemory: InMemory#Tx

    type I  = /*L*/InMemory.Txn
    type D  = /*L*/Durable .Txn
  }

//  implicit def inMemory(tx: T): InMemory#Tx = tx.inMemory
//  implicit def durable (tx: T): Durable #Tx = tx.durable
}

trait Confluent extends ConfluentLike[Confluent.Txn] with synth.Sys {
  protected type S  = Confluent
  type T            = Confluent .Txn
  type D            = Durable   .Txn
  type I            = InMemory  .Txn

  override def durable : Durable
  override def inMemory: InMemory
}