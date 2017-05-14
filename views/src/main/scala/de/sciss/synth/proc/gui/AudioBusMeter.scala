/*
 *  AudioBusMeter.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.gui

import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.{Txn, AudioBus, Group}
import impl.AudioBusMeterImpl
import de.sciss.synth.AddAction

import scala.collection.immutable.{Seq => ISeq}
import scala.swing.Component

object AudioBusMeter {
  /** Specification of a meter strip.
    *
    * @param bus        the audio bus to meter
    * @param target     the target point at which the meter synth will sit
    * @param addAction  the relation of the meter synth with respect to its target node.
    */
  final case class Strip(bus: AudioBus, target: Group, addAction: AddAction)

  /** Creates a new audio bus meter for a given list of strips.
    *
    * @param strips the buses and targets to meter. It is possible to mix servers.
    */
  def apply(strips: ISeq[Strip])(implicit tx: Txn): AudioBusMeter = {
    val res = new AudioBusMeterImpl(strips)
    res.init()
    res
  }
}
trait AudioBusMeter extends Disposable[Txn] {
  /** The buses and targets to meter. */
  def strips: ISeq[AudioBusMeter.Strip]

  /** The swing component showing the meter. */
  def component: Component
}