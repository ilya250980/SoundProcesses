/*
 *  AuralSystemTxBridge.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Server, Sys, Txn}
import de.sciss.synth.proc.{AuralContext, AuralSystem, SoundProcesses, Universe}

/** An `AuralSystem.Client` that issues full transactions and creates an `AuralContext` */
trait AuralSystemTxBridge[S <: Sys[S]] extends AuralSystem.Client with Disposable[S#Tx] {
  // ---- abstract ----

  implicit val universe: Universe[S]

  protected def auralStartedTx()(implicit tx: S#Tx, auralContext: AuralContext[S]): Unit

  protected def auralStoppedTx()(implicit tx: S#Tx): Unit

  // ---- impl ----

  final def connectAuralSystem()(implicit tx: S#Tx): this.type = {
    import universe.auralSystem
    auralSystem.addClient(this)
    auralSystem.serverOption.foreach { server =>
      auralStartedTx(server)
    }
    this
  }

  final def disconnectAuralSystem()(implicit tx: S#Tx): Unit = {
    import universe.auralSystem
    auralSystem.removeClient(this)
  }

  final def auralStarted(server: Server)(implicit tx: Txn): Unit = {
    // XXX TODO -- what was the reasoning for the txn decoupling?
    // (perhaps the discrepancy between Txn and S#Tx ?)
    tx.afterCommit {
      import universe.cursor
      SoundProcesses.atomic { implicit tx: S#Tx =>
        auralStartedTx(server)
      }
    }
  }

  final def auralStartedTx(server: Server)(implicit tx: S#Tx): Unit = {
    implicit val auralContext: AuralContext[S] = AuralContext(server)
    auralStartedTx()
  }

  final def auralStopped()(implicit tx: Txn): Unit =
    tx.afterCommit {
      import universe.cursor
      SoundProcesses.atomic { implicit tx: S#Tx =>
        auralStoppedTx()
      }
    }
}
