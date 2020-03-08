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

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.DataStore
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.synth.proc.Durable

import scala.concurrent.stm.InTxn

private[proc] object DurableImpl {
  def apply(factory: DataStore.Factory, mainName: String): Durable = {
    val mainStore   = factory.open(mainName)
    new System(mainStore)
  }

  def apply(mainStore: DataStore): Durable = new System(mainStore)

  private final class TxnImpl(val system: System, val systemTimeNanoSec: Long, val peer: InTxn)
    extends stm.impl.DurableImpl.TxnMixin[Durable]
    with TxnFullImpl[Durable] with Durable.Txn {

    lazy val inMemory: /* evt. */ InMemory#Tx = system.inMemory.wrap(peer)

    override def toString = s"proc.Durable#Tx@${hashCode.toHexString}"
  }

  private final class System(val store: DataStore)
    extends stm.impl.DurableImpl.Mixin[Durable, /* stm. */ InMemory]
    with Durable {
    // with evt.impl.ReactionMapImpl.Mixin[Durable] {

    type S = Durable

    val inMemory: /* evt. */ InMemory = /* evt. */ InMemory()
    def inMemoryTx(tx: Tx): I#Tx = tx.inMemory

    def wrap(peer: InTxn, systemTimeNanos: Long): S#Tx = new TxnImpl(this, systemTimeNanos, peer)

    override def toString = s"proc.Durable@${hashCode.toHexString}"
  }
}