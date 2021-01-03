/*
 *  SynthImpl.scala
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

package de.sciss.lucre.synth
package impl

import de.sciss.synth.{AddAction, ControlSet, Synth => SSynth}

import scala.collection.immutable.{Seq => ISeq}

final case class SynthImpl(peer: SSynth, definition: SynthDef) extends NodeImpl with Synth {
  override def toString = s"Synth(id=${peer.id}, def=${definition.name})"

  def server: Server = definition.server

  def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
          (implicit tx: RT): Unit = {

    val s = server
    requireOffline()
    if (target.server != s || !target.isOnline)
      throw new IllegalStateException(s"Target $target must be running and using the same server")

    if (dependencies.nonEmpty) {
      dependencies.foreach(r => if (r.server != s || !r.isOnline)
        throw new IllegalStateException(s"Dependency $r must be running and using the same server")
      )
    }
    tx.addMessage(this, peer.newMsg(definition.name, target.peer, args, addAction),
      dependencies = target :: definition :: dependencies)

    setOnline(value = true)
  }
}