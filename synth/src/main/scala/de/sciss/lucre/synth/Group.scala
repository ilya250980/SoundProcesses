/*
 *  Group.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.synth.impl.{GroupImpl => Impl}
import de.sciss.synth.{AddAction, addToHead, Group => SGroup}

object Group {
  def apply(server: Server)(implicit tx: RT): Group = {
    val nodeId = server.nextNodeId()
    Impl(server, SGroup(server.peer, nodeId))(online0 = false)
  }

  def play(target: Node, addAction: AddAction = addToHead)(implicit tx: RT): Group = {
    val g = apply(target.server)
    g.play(target, addAction)
    g
  }

  private[synth] def wrap(server: Server, peer: SGroup): Group = {
    require(server.peer == peer.server)
    Impl(server, peer)(online0 = true)
  }
}

trait Group extends Node {
  def peer: SGroup
  def freeAll()(implicit tx: RT): Unit

  def play(target: Node, addAction: AddAction)(implicit tx: RT): Unit
}