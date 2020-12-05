/*
 *  DurableImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.DataStore
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.impl.{DurableImpl => LDurableImpl}
import de.sciss.proc.Durable

import scala.concurrent.stm.InTxn

private[proc] object DurableImpl {
  def apply(factory: DataStore.Factory, mainName: String): Durable = {
    val mainStore   = factory.open(mainName)
    new System(mainStore)
  }

  def apply(mainStore: DataStore): Durable = new System(mainStore)

  private final class TxnImpl(val system: System, val systemTimeNanoSec: Long, val peer: InTxn)
    extends LDurableImpl.TxnMixin[Durable.Txn, InMemory.Txn]
    with TxnFullImpl[Durable.Txn] with Durable.Txn {

    lazy val inMemory: InMemory.Txn = system.inMemory.wrap(peer)

    implicit def inMemoryBridge: Durable.Txn => InMemory.Txn = _.inMemory

    override def toString = s"proc.Durable#Tx@${hashCode.toHexString}"
  }

  private final class System(val store: DataStore)
    extends LDurableImpl.Mixin[Durable.Txn, InMemory.Txn]
    with Durable {
    // with evt.impl.ReactionMapImpl.Mixin[Durable] {

    type S = Durable

    val inMemory: /* evt. */ InMemory = /* evt. */ InMemory()

//    def inMemoryTx(tx: Tx): I#Tx = tx.inMemory

    def wrap(peer: InTxn, systemTimeNanos: Long): T = new TxnImpl(this, systemTimeNanos, peer)

    override def toString = s"proc.Durable@${hashCode.toHexString}"
  }
}