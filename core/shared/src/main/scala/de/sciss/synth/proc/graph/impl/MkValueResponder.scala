/*
 *  MkValueResponder.scala
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

package de.sciss.synth.proc.graph.impl

import de.sciss.lucre.expr.graph.{Const => ExConst, Var => ExVar}
import de.sciss.lucre.synth.{RT, Synth, Txn}
import de.sciss.proc.{AuralContext, SoundProcesses}
import de.sciss.synth.GE
import de.sciss.{osc, synth}

import scala.collection.immutable.{IndexedSeq => Vec}

object MkValueResponder {
  // via SendReply
  private def replyName(key: String): String = s"/$$mk_$key"

  def makeUGen(trig: GE, values: GE, key: String): Unit = {
    import synth._
    import ugen._
    SendReply.kr(trig = trig, values = values, msgName = replyName(key), id = 0)
    ()
  }

  var DEBUG = false
}
final class MkValueResponder[T <: Txn[T], A](vr: ExVar.Expanded[T, A], key: String, protected val synth: Synth)
                                            (implicit context: AuralContext[T])
  extends SendReplyResponder {

  import MkValueResponder._

  private[this] val Name    = replyName(key)
  private[this] val NodeId  = synth.peer.id

  private def trySet(value: Any): Unit = {
    import context.universe.cursor
    SoundProcesses.step[T](s"MkValueResponder($synth, $key)") { implicit tx: T =>
      vr.fromAny.fromAny(value).foreach { valueT =>
        vr.update(new ExConst.Expanded(valueT))
      }
    }
  }

  protected val body: Body = {
    case osc.Message(Name, NodeId, 0, v0: Float) => trySet(v0.toDouble)
    case osc.Message(Name, NodeId, 0, raw @ _*) =>
      if (DEBUG) println(s"MkValueResponder($key, $NodeId) - received trigger")
      // logAural(m.toString)
      val values: Vec[Double] = raw match {
        case rawV: Vec[Any] =>
          rawV.collect {
            case f: Float => f.toDouble
          }

        case _ =>
          raw.iterator.collect {
            case f: Float => f.toDouble
          }.toIndexedSeq
      }
      trySet(values)
  }

  protected def added()(implicit tx: RT): Unit = ()
}