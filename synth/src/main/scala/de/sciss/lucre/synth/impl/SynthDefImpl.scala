/*
 *  SynthDefImpl.scala
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
package impl

import java.io.File

import de.sciss.synth.message.SynthDefLoad
import de.sciss.synth.{SynthDef => SSynthDef}

final case class SynthDefImpl(server: Server, peer: SSynthDef) extends ResourceImpl with SynthDef {
  override def toString = s"SynthDef(${peer.name})"

  def name: String = peer.name

  /** Queues a receive message.
    * If the SynthDef is too large, it will be written to a temporary
    * file and `/d_load` used instead.
    */
  def recv()(implicit tx: Txn): Unit = {
    requireOffline()
    val mRecv     = peer.recvMsg
    val bndlSize  = ((mRecv.bytes.limit() + 7) & ~3) + 32  // [ "#bundle" 8, timetag 8, sz 4, [ "/d_recv" 8, tags 4, byte-buf ]]
    val m = if (bndlSize <= server.maxPacketSize || !server.peer.isLocal) mRecv else {
      val file = File.createTempFile("temp", s".${SSynthDef.extension}")
      val path = file.getAbsolutePath
      file.deleteOnExit()
      // not too pretty doing this inside a transaction...
      SSynthDef.write(path, peer :: Nil)
      SynthDefLoad(path, None)
    }

    tx.addMessage(this, m)
    setOnline(value = true)
  }

  def dispose()(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.freeMsg)
    setOnline(value = false)
  }
}