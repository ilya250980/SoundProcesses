/*
 *  StopSelfResponder.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package graph
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Sys, Txn, Node}
import de.sciss.osc

final class StopSelfResponder[S <: Sys[S]](view: AuralView[S, Nothing], protected val synth: Node)
                                          (implicit cursor: stm.Cursor[S])
  extends SendReplyResponder {

  private[this] val NodeId = synth.peer.id

  protected val body: Body = {
    case osc.Message(StopSelf.replyName, NodeId, 0, _) =>
      SoundProcesses.atomic { implicit tx: S#Tx =>
        view.stop()
      }
  }

  protected def added()(implicit tx: Txn): Unit = ()
}
