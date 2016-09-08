/*
 *  Resource.scala
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

package de.sciss.lucre
package synth

import de.sciss.lucre.stm.Disposable

object Resource {
  type TimeStamp = Int

  //  object Source {
  //    def apply(resource: Resource): Source = new Impl(resource)
  //
  //    private final class Impl(_res: Resource) extends Source {
  //      override def toString = s"Resource.Source($_res)"
  //
  //      def resource(implicit tx: Txn): Resource = _res
  //    }
  //  }
  //  trait Source {
  //    def resource(implicit tx: Txn): Resource
  //  }

  // def User() = ()

  /** Forwards the resource API to a peer resource */
  trait Proxy extends Resource {
    protected def resourcePeer: Resource

    def dispose()(implicit tx: Txn): Unit = resourcePeer.dispose()

    private[synth] def timeStamp_=(value: TimeStamp)(implicit tx: Txn): Unit      = resourcePeer.timeStamp = value
    private[synth] def timeStamp                    (implicit tx: Txn): TimeStamp = resourcePeer.timeStamp

    def server: Server = resourcePeer.server

    def isOnline(implicit tx: Txn): Boolean = resourcePeer.isOnline
  }
}

trait Resource extends Disposable[Txn] {
  import Resource.TimeStamp

  def isOnline(implicit tx: Txn): Boolean

  def server: Server

  private[synth] def timeStamp                    (implicit tx: Txn): TimeStamp
  private[synth] def timeStamp_=(value: TimeStamp)(implicit tx: Txn): Unit
}