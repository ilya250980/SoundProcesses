/*
 *  ConfluentImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.DataStore
import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.{confluent, stm}

import scala.concurrent.stm.InTxn

/*
 * XXX TODO: don't repeat yourself. Should factor out more stuff in ConfluentReactiveImpl
 */
private[proc] object ConfluentImpl {
  private type S = Confluent

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

  private sealed trait TxnImpl extends Confluent.Txn with confluent.impl.TxnMixin[S] with TxnFullImpl[S] {
    final lazy val inMemory: /* evt. */ InMemory#Tx = system.inMemory.wrap(peer)
  }

  private final class RegularTxn(val system: S, val durable: /* evt. */ Durable#Tx,
                                 val inputAccess: S#Acc, val isRetroactive: Boolean,
                                 val cursorCache: confluent.Cache[S#Tx])
    extends confluent.impl.RegularTxnMixin[S, stm.Durable] with TxnImpl {

    lazy val peer: InTxn = durable.peer
  }

  private final class RootTxn(val system: S, val peer: InTxn)
    extends confluent.impl.RootTxnMixin[S, stm.Durable] with TxnImpl {

    lazy val durable: /* evt. */ Durable#Tx = {
      log("txn durable")
      system.durable.wrap(peer)
    }
  }

  private final class System(protected val storeFactory: DataStore.Factory,
                             val durable: /* evt. */ Durable)
    extends confluent.impl.Mixin[S]
    with Confluent {

    def inMemory            : I     = durable.inMemory
    def durableTx (tx: S#Tx): D#Tx  = tx.durable
    def inMemoryTx(tx: S#Tx): I#Tx  = tx.inMemory

    protected def wrapRegular(dtx: /* evt. */ Durable#Tx, inputAccess: S#Acc, retroactive: Boolean,
                              cursorCache: confluent.Cache[S#Tx]) =
      new RegularTxn(this, dtx, inputAccess, retroactive, cursorCache)

    protected def wrapRoot(peer: InTxn) = new RootTxn(this, peer)
  }
}