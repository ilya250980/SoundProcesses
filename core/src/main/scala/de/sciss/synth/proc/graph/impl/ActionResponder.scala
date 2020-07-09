/*
 *  ActionResponder.scala
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

import de.sciss.lucre.expr.{Context, IExprAsRunnerMap}
import de.sciss.lucre.expr.graph.Const
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, UndoManager}
import de.sciss.lucre.synth.{Node, Sys, Txn}
import de.sciss.synth.proc.{AuralContext, Runner, SoundProcesses}
import de.sciss.synth.{GE, proc}
import de.sciss.{osc, synth}

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
  private[this] val NodeId  = synth.peer.id

//  protected val body: Body = {
//    case osc.Message(Name, NodeId, 0, raw @ _*) =>
//      if (DEBUG) println(s"ActionResponder($key, $NodeId) - received trigger")
//      // logAural(m.toString)
//      val values: Vec[Float] = raw match {
//        case rawV: Vec[_] =>
//          rawV.collect {
//            case f: Float => f
//          }
//
//        case _ =>
//          raw.iterator.collect {
//            case f: Float => f
//          }.toIndexedSeq
//      }
//      import context.universe
//      import context.universe.cursor
//      SoundProcesses.step(s"ActionResponder($synth, $key)") { implicit tx: S#Tx =>
//        val invoker = objH()
//        invoker.attr.$[proc.ActionRaw](key).foreach { action =>
//          if (DEBUG) println("...and found action")
//          val au = proc.Action.Universe[S](action, invoker = Some(invoker),
//            value = proc.ActionRaw.FloatVector(values))
//          action.execute(au)
//        }
//      }
//  }

  protected val body: Body = {
    case osc.Message(Name, NodeId, 0, raw @ _*) =>
      if (DEBUG) println(s"ActionResponder($key, $NodeId) - received trigger")
      // logAural(m.toString)
      val values: Vec[Double] = raw match {
        case rawV: Vec[_] =>
          rawV.collect {
            case f: Float => f.toDouble
          }

        case _ =>
          raw.iterator.collect {
            case f: Float => f.toDouble
          }.toIndexedSeq
      }
      import context.universe
      import context.universe.cursor
      SoundProcesses.step(s"ActionResponder($synth, $key)") { implicit tx: S#Tx =>
        val invoker = objH()
        invoker.attr.$[proc.Action](key).foreach { action =>
          val r = Runner(action)
          if (DEBUG) println("...and found action")
          val selfH = objH  // XXX TODO -- or should that be the `Action`?
          import universe.workspace
          implicit val undo: UndoManager[S] = UndoManager.dummy
          implicit val ctx: Context[S] = Context[S](selfH = Some(selfH))
          import ctx.targets
          r.prepare(new IExprAsRunnerMap[S](
            new Const.Expanded[S, (String, Vec[Double])](("value", values)) :: Nil, tx
          ))
          r.run()
          r.dispose()
          ctx.dispose()
        }
      }
  }

  protected def added()(implicit tx: Txn): Unit = ()
}