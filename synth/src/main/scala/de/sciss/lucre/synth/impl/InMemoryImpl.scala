/*
 *  InMemoryImpl.scala
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

package de.sciss.lucre.synth.impl

import de.sciss.lucre.impl.ReactionMapImpl
import de.sciss.lucre
import de.sciss.lucre.synth.InMemory

import scala.concurrent.stm.InTxn

object InMemoryImpl {
  def apply(): InMemory = new System

  private final class TxnImpl(val system: InMemory, val systemTimeNanoSec: Long, val peer: InTxn)
    extends lucre.impl.InMemoryImpl.TxnMixin[InMemory.Txn]
    with TxnFullImpl[InMemory.Txn] with InMemory.Txn {

    override def toString = s"proc.InMemory#Tx@${hashCode.toHexString}"

    def inMemory: InMemory.Txn = this

    def inMemoryBridge: InMemory.Txn => InMemory.Txn = tx => tx
  }

  private final class System
    extends lucre.impl.InMemoryImpl.Mixin[InMemory.Txn]
    with InMemory with ReactionMapImpl.Mixin[InMemory.Txn] {

    type S = InMemory

//    def inMemory: I = this
//    def inMemoryTx(tx: T): T = tx

    def wrap(peer: InTxn, systemTimeNanos: Long): T =
      new TxnImpl(this, systemTimeNanos, peer)

    override def toString = s"proc.InMemory@${hashCode.toHexString}"
  }
}