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

import de.sciss.lucre.synth.InMemory
import de.sciss.lucre.synth.impl.TxnFullImpl
import de.sciss.lucre.{DataStore, confluent, Durable => LDurable, InMemory => LInMemory}
import de.sciss.proc.{Confluent, Durable}
import de.sciss.proc.SoundProcesses.log

import scala.concurrent.stm.InTxn

/*
 * XXX TODO: don't repeat yourself. Should factor out more stuff in ConfluentReactiveImpl
 */
private[proc] object ConfluentImpl {
  private type S = Confluent
  private type T = Confluent.Txn
//  private type I = InMemory .Txn
  private type D = Durable  .Txn

  def apply(storeFactory: DataStore.Factory): S = {
    // We can share the event store between confluent and durable, because there are
    // no key collisions. Durable uses 32-bit ints exclusively, and confluent maintains
    // a DurablePersistentMap, which uses 32 + 64 bit keys.

    // val durable = evt.Durable(storeFactory, eventName = "d-evt")
    val mainStoreD  = storeFactory.open("d-main")
    // val eventStore  = storeFactory.open("event", overwrite = true)  // shared between durable + confluent

    val durable     = /* evt. */ Durable(mainStore = mainStoreD)
    ??? // new System(storeFactory, durable)
  }

  private sealed trait TxnImpl extends Confluent.Txn
    with confluent.impl.TxnMixin[T, Durable.Txn, InMemory.Txn]
    with TxnFullImpl[T] { self =>

    override type D = Durable.Txn
    override type I = InMemory.Txn

    override def system: Confluent

    final lazy val inMemory: InMemory.Txn = system.inMemory.wrap(peer)

    final def inMemoryBridge : self.T => InMemory.Txn = _.inMemory
    final def durableBridge  : self.T => Durable .Txn = _.durable
  }

  private final class RegularTxn(val system: S, val durable: /* evt. */ Durable.Txn,
                                 val inputAccess: S#Acc, val isRetroactive: Boolean,
                                 val cursorCache: confluent.Cache[T],
                                 val systemTimeNanoSec: Long)
    extends confluent.impl.RegularTxnMixin[T, /*L*/Durable.Txn, /*L*/InMemory.Txn] with TxnImpl {

    lazy val peer: InTxn = durable.peer
  }

  private final class RootTxn(val system: S, val peer: InTxn)
    extends confluent.impl.RootTxnMixin[T, /*L*/Durable.Txn, /*L*/InMemory.Txn] with TxnImpl {

    def systemTimeNanoSec: Long = 0L

    lazy val durable: /* evt. */ Durable.Txn = {
      log.debug("txn durable")
      system.durable.wrap(peer)
    }
  }

//  private final class System(protected val storeFactory: DataStore.Factory,
//                             /*val*/ durable0: /* evt. */ Durable)
//    extends confluent.impl.Mixin[T]
//    with Confluent { self =>
//
//    override type T  = Durable.Txn // Sys.Txn[Durable] with evt.DurableLike.Txn[Durable]
//    override type I  = InMemory.Txn
//
//    def inMemory: InMemory = ??? // durable0.inMemory
//
//    def durableTx (tx: ConfluentImpl.T): ConfluentImpl.D = ??? // tx.durable
//
////    def inMemoryTx(tx: T): I#Tx  = tx.inMemory
//
////    protected def wrapRegular(dtx: /* evt. */ Durable.Txn, inputAccess: S#Acc, retroactive: Boolean,
////                              cursorCache: confluent.Cache[T], systemTimeNanos: Long) =
////      new RegularTxn(system = this, durable = dtx, inputAccess = inputAccess, isRetroactive = retroactive,
////        cursorCache = cursorCache, systemTimeNanoSec = systemTimeNanos)
//
//    protected def wrapRoot(peer: InTxn): ConfluentImpl.T = ??? // new RootTxn(this, peer)
//  }
}