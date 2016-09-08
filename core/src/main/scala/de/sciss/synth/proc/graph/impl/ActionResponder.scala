/*
 *  ActionResponder.scala
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

package de.sciss.synth.proc
package graph
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.{Node, Sys, Txn}
import de.sciss.synth.{GE, proc}
import de.sciss.{osc, synth}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object ActionResponder {
  // via SendReply
  private def replyName(key: String): String = s"/$$act_$key"

  def makeUGen(trig: GE, values: Option[GE], key: String): Unit = {
    import synth._
    import ugen._
    // we cannot make values.size zero, because then in the multi-channel expansion,
    // the SendReply collapses :)
    SendReply.kr(trig = trig, values = values.getOrElse(0) /* Vec.empty[GE] */ , msgName = replyName(key), id = 0)
  }

  var DEBUG = false
}
final class ActionResponder[S <: Sys[S]](objH: stm.Source[S#Tx, Obj[S]], key: String, protected val synth: Node)
                                        (implicit context: AuralContext[S])
  extends SendReplyResponder {

  import ActionResponder._

  private[this] val Name    = replyName(key)
  private[this] val NodeID  = synth.peer.id

  protected val body: Body = {
    case osc.Message(Name, NodeID, 0, raw @ _*) =>
      if (DEBUG) println(s"ActionResponder($key, $NodeID) - received trigger")
      // logAural(m.toString)
      val values: Vec[Float] = raw.collect {
        case f: Float => f
      } (breakOut)

      import context.scheduler.cursor
      SoundProcesses.atomic { implicit tx: S#Tx =>
        val invoker = objH()
        invoker.attr.$[proc.Action](key).foreach { action =>
          if (DEBUG) println("...and found action")
          val universe = proc.Action.Universe(action, context.workspaceHandle,
            invoker = Some(invoker), values = values)
          action.execute(universe)
        }
      }
  }

  protected def added()(implicit tx: Txn): Unit = ()
}