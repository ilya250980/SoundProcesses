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

package de.sciss.proc.impl

import de.sciss.lucre.confluent.Access
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.synth.{InMemory => InM}
import de.sciss.lucre.{DataStore, confluent}
import de.sciss.proc.SoundProcesses.log
import de.sciss.proc.{Confluent => Cf, Durable => Dur}

import scala.concurrent.stm.InTxn

/*
 * XXX TODO: don't repeat yourself. Should factor out more stuff in ConfluentReactiveImpl
 */
private[proc] object ConfluentImpl {
  def apply(storeFactory: DataStore.Factory): Cf = {
    // We can share the event store between confluent and durable, because there are
    // no key collisions. Durable uses 32-bit ints exclusively, and confluent maintains
    // a DurablePersistentMap, which uses 32 + 64 bit keys.

    // val durable = evt.Durable(storeFactory, eventName = "d-evt")
    val mainStoreD  = storeFactory.open("d-main")
    val durable     = Dur(mainStore = mainStoreD)
    new System(storeFactory, durable)
  }

  private sealed trait TxnImpl extends Cf.Txn
    with confluent.impl.TxnMixin[Cf.Txn, Dur.Txn, InM.Txn]
    with TxnFullImpl[Cf.Txn] { self =>

    override type D = Dur.Txn
    override type I = InM.Txn

    override def system: Cf

    final lazy val inMemory: InM.Txn = system.inMemory.wrap(peer)

    final def inMemoryBridge : T => InM.Txn = _.inMemory
    final def durableBridge  : T => Dur.Txn = _.durable
  }

  private final class RegularTxn(val system: Cf, val durable: /* evt. */ Dur.Txn,
                                 val inputAccess: Access[Cf.Txn], val isRetroactive: Boolean,
                                 val cursorCache: confluent.Cache[Cf.Txn],
                                 val systemTimeNanoSec: Long)
    extends confluent.impl.RegularTxnMixin[Cf.Txn, Dur.Txn, InM.Txn] with TxnImpl {

    lazy val peer: InTxn = durable.peer
  }

  private final class RootTxn(val system: Cf, val peer: InTxn)
    extends confluent.impl.RootTxnMixin[Cf.Txn, Dur.Txn, InM.Txn] with TxnImpl {

    def systemTimeNanoSec: Long = 0L

    lazy val durable: /* evt. */ Dur.Txn = {
      log.debug("txn durable")
      system.durable.wrap(peer)
    }
  }

  private final class System(protected val storeFactory: DataStore.Factory,
                             val durable: Dur)
    extends confluent.impl.Mixin[Cf.Txn]
    with Cf { self =>

    override type D  = Dur.Txn // Sys.Txn[Durable] with evt.DurableLike.Txn[Durable]
    override type I  = InM.Txn

    def inMemory: InM = durable.inMemory

    def durableTx (tx: T): D = tx.durable

//    def inMemoryTx(tx: T): I#Tx  = tx.inMemory

    protected def wrapRegular(dtx: Dur.Txn, inputAccess: Access[T], retroactive: Boolean,
                              cursorCache: confluent.Cache[T], systemTimeNanos: Long) =
      new RegularTxn(system = this, durable = dtx, inputAccess = inputAccess, isRetroactive = retroactive,
        cursorCache = cursorCache, systemTimeNanoSec = systemTimeNanos)

    protected def wrapRoot(peer: InTxn): T = new RootTxn(this, peer)
  }
}