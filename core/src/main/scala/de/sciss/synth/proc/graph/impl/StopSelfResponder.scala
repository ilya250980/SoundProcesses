/*
 *  StopSelfResponder.scala
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

package de.sciss.synth.proc.graph.impl

import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Node, Sys, Txn}
import de.sciss.osc
import de.sciss.synth.proc.graph.StopSelf
import de.sciss.synth.proc.{SoundProcesses, ViewBase}

final class StopSelfResponder[S <: Sys[S]](view: ViewBase[S], protected val synth: Node)
                                          (implicit cursor: stm.Cursor[S])
  extends SendReplyResponder {

  private[this] val NodeId = synth.peer.id

  protected val body: Body = {
    case osc.Message(StopSelf.replyName, NodeId, 0, _) =>
      SoundProcesses.step(s"StopSelfResponder($synth)") { implicit tx: S#Tx =>
        view.stop()
      }
  }

  protected def added()(implicit tx: Txn): Unit = ()
}
