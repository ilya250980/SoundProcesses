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

import de.sciss.lucre.Cursor
import de.sciss.lucre.synth.{Node, RT, Txn}
import de.sciss.osc
import de.sciss.synth.proc.graph.StopSelf
import de.sciss.proc.{SoundProcesses, ViewBase}

final class StopSelfResponder[T <: Txn[T]](view: ViewBase[T], protected val synth: Node)
                                          (implicit cursor: Cursor[T])
  extends SendReplyResponder {

  private[this] val NodeId = synth.peer.id

  protected val body: Body = {
    case osc.Message(StopSelf.replyName, NodeId, 0, _) =>
      SoundProcesses.step(s"StopSelfResponder($synth)") { implicit tx: T =>
        view.stop()
      }
  }

  protected def added()(implicit tx: RT): Unit = ()
}
