/*
 *  ResourceImpl.scala
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
package impl

import de.sciss.lucre.Log.{synth => log}

import scala.concurrent.stm.Ref

trait ResourceImpl extends Resource {
  import Resource.TimeStamp

  private val timeStampRef = Ref(0)

  protected def online0: Boolean = false

  private val stateOnline = Ref(initialValue = online0)

  final def isOnline(implicit tx: RT): Boolean = stateOnline.get(tx.peer)
  final protected def setOnline(value: Boolean)(implicit tx: RT): Unit = stateOnline.set(value)(tx.peer)

  final def timeStamp(implicit tx: RT): TimeStamp = timeStampRef.get(tx.peer)

  final def timeStamp_=(value: TimeStamp)(implicit tx: RT): Unit = timeStampRef.set(value)(tx.peer)

  final protected def requireOnline ()(implicit tx: RT): Unit = require( isOnline, "must be online")
  final protected def requireOffline()(implicit tx: RT): Unit = require(!isOnline, "must be offline")

  // final protected def require(p: Boolean): Unit = require(p, "")

  final protected def require(p: Boolean, message: => String): Unit = {
    if (!p) {
      val user  = message
      val msg   = if (user.isEmpty) s"$this is not in required state" else s"$this - $user"
      log.debug(msg)
      throw new IllegalStateException(msg)
    }
  }

  //   final def addDependent( dependent: Resource )( implicit tx: Txn ): Unit = {
  //      require( dependent.server == server, "Dependency uses divergent server : " + dependent )
  //      dependentsRef.add( dependent )( tx.peer )
  //   }
  //
  //   final def removeDependent( dependent: Resource )( implicit tx: Txn ): Unit = {
  //      dependentsRef.remove( dependent )( tx.peer )
  //   }
}