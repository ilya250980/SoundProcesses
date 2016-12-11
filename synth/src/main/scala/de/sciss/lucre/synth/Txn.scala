/*
 *  Txn.scala
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

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.impl.TxnPlainImpl
import de.sciss.osc
import de.sciss.synth.message

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.InTxn

object Txn {
  def wrap(itx: InTxn): Txn = new TxnPlainImpl(itx)

  type Message = osc.Message with message.Send

  /** A data type encapsulating an outgoing OSC bundle for this transaction.
    */
  final class Bundle(val stamp: Int, val msgs: Vec[Message]) {
    // def copy(newMsgs: Vec[Message]): Bundle = new Bundle(stamp, newMsgs)
    def append(msg: Message): Bundle = new Bundle(stamp, msgs :+ msg)

    /** A bundle depends on messages with any smaller time stamp (this stamp minus one). */
    def depStamp: Int = stamp - 1
  }

  type Bundles = Vec[Bundle]
}

/** The `Txn` trait is declared without representation type parameter in order to keep the real-time sound
  * synthesis API clutter free. The sound synthesis is always ephemeral, so does not need to know anything
  * about the underlying system. What the process transaction provides is a package private
  * `addMessage` method for staging OSC messages which are flushed at the end of a successful transaction.
  */
trait Txn extends TxnLike {
  def addMessage(resource: Resource, m: osc.Message with message.Send, dependencies: Seq[Resource] = Nil): Unit
}