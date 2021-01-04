/*
 *  SynthDefImpl.scala
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

import de.sciss.synth.{SynthDef => SSynthDef}

final case class SynthDefImpl(server: Server, peer: SSynthDef) extends ResourceImpl with SynthDef with SynthDefPlatform {
  override def toString = s"SynthDef(${peer.name})"

  def name: String = peer.name

  /** Queues a receive message.
    * If the SynthDef is too large, it will be written to a temporary
    * file and `/d_load` used instead.
    */
  def recv()(implicit tx: RT): Unit = {
    requireOffline()
    val mRecv     = peer.recvMsg
    val bndlSize  = ((mRecv.bytes.limit() + 7) & ~3) + 32  // [ "#bundle" 8, timetag 8, sz 4, [ "/d_recv" 8, tags 4, byte-buf ]]
    val m = if (bndlSize <= server.maxPacketSize || !server.peer.isLocal) mRecv else loadDef(peer)
    tx.addMessage(this, m)
    setOnline(value = true)
  }

  def dispose()(implicit tx: RT): Unit = {
    requireOnline()
    tx.addMessage(this, peer.freeMsg)
    setOnline(value = false)
  }
}