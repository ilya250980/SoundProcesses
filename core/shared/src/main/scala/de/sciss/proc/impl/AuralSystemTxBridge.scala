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
import de.sciss.proc.AuralSystem.{Running, Stopped}
import de.sciss.proc.{AuralContext, SoundProcesses, Universe}

import scala.concurrent.stm.Ref

/** An `AuralSystem.Client` that issues full transactions and creates an `AuralContext` */
trait AuralSystemTxBridge[T <: Txn[T]] extends Disposable[T] {
  // ---- abstract ----

  implicit val universe: Universe[T]

  protected def auralStartedTx()(implicit tx: T, auralContext: AuralContext[T]): Unit

  protected def auralStoppedTx()(implicit tx: T): Unit

  // ---- impl ----

  private[this] val obsRef = Ref(Disposable.empty[T])

  final def connectAuralSystem()(implicit tx: T): this.type = {
    import universe.auralSystem
    val obs = auralSystem.react { implicit tx => {
      case Running(server)  => auralStarted(server)
      case Stopped          => auralStopped()
      case _ =>
    }}
    obsRef.set(obs)(tx.peer)
    auralSystem.serverOption.foreach { server =>
      auralStartedTx(server)
    }
    this
  }

  final def disconnectAuralSystem()(implicit tx: T): Unit =
    obsRef.swap(Disposable.empty)(tx.peer).dispose()

  private def auralStarted(server: Server)(implicit tx: RT): Unit = {
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

  private def auralStopped()(implicit tx: RT): Unit =
    tx.afterCommit {
      import universe.cursor
      SoundProcesses.step[T]("auralStopped") { implicit tx: T =>
        auralStoppedTx()
      }
    }
}
