/*
 *  SendReplyResponder.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package graph
package impl

import de.sciss.lucre.synth.{DynamicUser, Node, Txn}
import de.sciss.osc

import scala.concurrent.stm.Ref

trait SendReplyResponder extends DynamicUser {

  protected type Body = PartialFunction[osc.Message, Unit]

  // ---- abstract ----

  protected def synth   : Node

  protected def added()(implicit tx: Txn): Unit

  protected def body: Body

  // ---- impl ----

  private[this] lazy val trigResp = message.Responder(synth.server.peer)(body)

  private[this] val _added = Ref(initialValue = false)

  final def add()(implicit tx: Txn): Unit = if (!_added.swap(true)(tx.peer)) {
    trigResp.add()
    // Responder.add is non-transactional. Thus, if the transaction fails, we need to remove it.
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.remove()
    } (tx.peer)

    added()
    // synth.onEnd(trigResp.remove())
  }

  final def remove()(implicit tx: Txn): Unit = if (_added.swap(false)(tx.peer)) {
    trigResp.remove()
    scala.concurrent.stm.Txn.afterRollback { _ =>
      trigResp.add()
    } (tx.peer)
  }
}