/*
 *  ActionImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.{Action, Universe}

import scala.collection.immutable.{IndexedSeq => Vec}

// XXX TODO --- complete DRY with Control
object ActionImpl {
  private final val SER_VERSION = 0x4163  // "Ac"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Action[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Action[S]] {
    def tpe: Obj.Type = Action
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action[S] = {
    val targets = Targets.read(in, access)
    val serVer  = in.readShort()
    if (serVer == SER_VERSION) {
      new Read(in, access, targets)
    } else {
      sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }
  }

  // ---- node impl ----

  private sealed trait Impl[S <: Sys[S]]
    extends Action[S] with evt.impl.SingleNode[S, Action.Update[S]] {
    proc =>

    // --- impl ----

    final def tpe: Obj.Type = Action

    override def toString: String = s"Action$id"

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out] = Targets[Out]
        val graph: Action.GraphObj.Var[Out] = context(proc.graph)

        connect()
      }

    def execute(universe: Universe[S])(implicit tx: S#Tx): Unit = ???

    // ---- key maps ----

    def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

    final def connect()(implicit tx: S#Tx): this.type = {
      graph.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      graph.changed -/-> changed
    }

    object changed extends Changed
      with evt.impl.Generator[S, Action.Update[S]] {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Action.Update[S]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Action.Update[S]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Action.Change[S]]) { u =>
          Vector(Action.GraphChange(u))
        }

        val seq3 = stateOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Action.Update(proc, seq3))
      }
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      graph.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      graph.dispose()
    }
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets: Targets[S] = Targets(tx0)
    val graph: Action.GraphObj.Var[S] = Action.GraphObj.newVar(Action.GraphObj.empty)
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    val graph: Action.GraphObj.Var[S] = Action.GraphObj.readVar(in, access)
  }
}