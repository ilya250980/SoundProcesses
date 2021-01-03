/*
 *  AuralOutput.scala
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

package de.sciss.proc

import de.sciss.lucre.synth.{AudioBus, NodeRef}
import de.sciss.lucre.{Disposable, Observable, Txn, synth}
import de.sciss.proc.impl.AuralOutputImpl

object AuralOutput {
  /** Creates a new aural scan view and registers it with the context under `scan.id`. */
  def apply[T <: synth.Txn[T]](view: AuralObj.Proc[T], output: Proc.Output[T], bus: AudioBus)
                        (implicit tx: T, context: AuralContext[T]): AuralOutput.Owned[T] =
    AuralOutputImpl(view = view, output = output, bus = bus)

  trait Owned[T <: Txn[T]] extends AuralOutput[T] {
    def stop()(implicit tx: T): Unit
    def play(n: NodeRef)(implicit tx: T): Unit
  }

  sealed trait Update
  case class  Play(n: NodeRef) extends Update
  case object Stop             extends Update
}

trait AuralOutput[T <: Txn[T]] extends Disposable[T] with Observable[T, AuralOutput.Update] {
  def view: AuralObj.Proc[T]

  def key : String
  def bus : AudioBus
}
