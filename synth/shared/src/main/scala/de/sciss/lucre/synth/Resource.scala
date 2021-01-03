/*
 *  Resource.scala
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

import de.sciss.lucre.Disposable

object Resource {
  type TimeStamp = Int

  /** Forwards the resource API to a peer resource */
  trait Proxy extends Resource {
    protected def resourcePeer: Resource

    def dispose()(implicit tx: RT): Unit = resourcePeer.dispose()

    private[synth] def timeStamp_=(value: TimeStamp)(implicit tx: RT): Unit      = resourcePeer.timeStamp = value
    private[synth] def timeStamp                    (implicit tx: RT): TimeStamp = resourcePeer.timeStamp

    def server: Server = resourcePeer.server

    def isOnline(implicit tx: RT): Boolean = resourcePeer.isOnline
  }
}

trait Resource extends Disposable[RT] {
  import Resource.TimeStamp

  def isOnline(implicit tx: RT): Boolean

  def server: Server

  // logical time stamp of last modification; initially zero, negative when disposed
  private[synth] def timeStamp                    (implicit tx: RT): TimeStamp
  private[synth] def timeStamp_=(value: TimeStamp)(implicit tx: RT): Unit
}