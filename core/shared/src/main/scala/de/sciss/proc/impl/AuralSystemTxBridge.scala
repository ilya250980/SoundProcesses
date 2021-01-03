/*
 *  AuralSystemTxBridge.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc.impl

import de.sciss.lucre.Disposable
import de.sciss.lucre.synth.{RT, Server, Txn}
import de.sciss.proc.{AuralContext, AuralSystem, SoundProcesses, Universe}

/** An `AuralSystem.Client` that issues full transactions and creates an `AuralContext` */
trait AuralSystemTxBridge[T <: Txn[T]] extends AuralSystem.Client with Disposable[T] {
  // ---- abstract ----

  implicit val universe: Universe[T]

  protected def auralStartedTx()(implicit tx: T, auralContext: AuralContext[T]): Unit

  protected def auralStoppedTx()(implicit tx: T): Unit

  // ---- impl ----

  final def connectAuralSystem()(implicit tx: T): this.type = {
    import universe.auralSystem
    auralSystem.addClient(this)
    auralSystem.serverOption.foreach { server =>
      auralStartedTx(server)
    }
    this
  }

  final def disconnectAuralSystem()(implicit tx: T): Unit = {
    import universe.auralSystem
    auralSystem.removeClient(this)
  }

  final def auralStarted(server: Server)(implicit tx: RT): Unit = {
    // The reasoning for the txn decoupling
    // is the discrepancy between Txn and T
    tx.afterCommit {
      import universe.cursor
      SoundProcesses.step[T]("auralStarted") { implicit tx: T =>
        auralStartedTx(server)
      }
    }
  }

  final def auralStartedTx(server: Server)(implicit tx: T): Unit = {
    implicit val auralContext: AuralContext[T] = AuralContext(server)
    auralStartedTx()
  }

  final def auralStopped()(implicit tx: RT): Unit =
    tx.afterCommit {
      import universe.cursor
      SoundProcesses.step[T]("auralStopped") { implicit tx: T =>
        auralStoppedTx()
      }
    }
}
