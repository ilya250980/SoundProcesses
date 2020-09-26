/*
 *  ConfluentImpl.scala
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

import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.{DataStore, confluent, Durable => LDurable}
import de.sciss.synth.proc.{Confluent, Durable, log}

import scala.concurrent.stm.InTxn

/*
 * XXX TODO: don't repeat yourself. Should factor out more stuff in ConfluentReactiveImpl
 */
private[proc] object ConfluentImpl {
  private type S = Confluent
  private type T = Confluent.Txn

  def apply(storeFactory: DataStore.Factory): S = {
    // We can share the event store between confluent and durable, because there are
    // no key collisions. Durable uses 32-bit ints exclusively, and confluent maintains
    // a DurablePersistentMap, which uses 32 + 64 bit keys.

    // val durable = evt.Durable(storeFactory, eventName = "d-evt")
    val mainStoreD  = storeFactory.open("d-main")
    // val eventStore  = storeFactory.open("event", overwrite = true)  // shared between durable + confluent

    val durable     = /* evt. */ Durable(mainStore = mainStoreD)
    new System(storeFactory, durable)
  }

  private sealed trait TxnImpl extends Confluent.Txn with confluent.impl.TxnMixin[T] with TxnFullImpl[T] {
    override def system: Confluent

    final lazy val inMemory: InMemory.Txn = system.inMemory.wrap(peer)

    final def inMemoryBridge : T => InMemory.Txn = _.inMemory
    final def durableBridge  : T => Durable .Txn = _.durable
  }

  private final class RegularTxn(val system: S, val durable: /* evt. */ Durable.Txn,
                                 val inputAccess: S#Acc, val isRetroactive: Boolean,
                                 val cursorCache: confluent.Cache[T],
                                 val systemTimeNanoSec: Long)
    extends confluent.impl.RegularTxnMixin[T, LDurable.Txn] with TxnImpl {

    lazy val peer: InTxn = durable.peer
  }

  private final class RootTxn(val system: S, val peer: InTxn)
    extends confluent.impl.RootTxnMixin[T, LDurable.Txn] with TxnImpl {

    def systemTimeNanoSec: Long = 0L

    lazy val durable: /* evt. */ Durable.Txn = {
      log("txn durable")
      system.durable.wrap(peer)
    }
  }

  private final class System(protected val storeFactory: DataStore.Factory,
                             val durable: /* evt. */ Durable)
    extends confluent.impl.Mixin[T]
    with Confluent {

    def inMemory: InMemory = durable.inMemory

    def durableTx (tx: T): D  = tx.durable

//    def inMemoryTx(tx: T): I#Tx  = tx.inMemory

    protected def wrapRegular(dtx: /* evt. */ Durable.Txn, inputAccess: S#Acc, retroactive: Boolean,
                              cursorCache: confluent.Cache[T], systemTimeNanos: Long) =
      new RegularTxn(system = this, durable = dtx, inputAccess = inputAccess, isRetroactive = retroactive,
        cursorCache = cursorCache, systemTimeNanoSec = systemTimeNanos)

    protected def wrapRoot(peer: InTxn) = new RootTxn(this, peer)
  }
}