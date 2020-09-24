/*
 *  ActionImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{GeneratorEvent, ObjFormat, SingleEventNode}
import de.sciss.lucre.{AnyTxn, Copy, Elem, Obj, Pull, Txn}
import de.sciss.serial.{DataInput, DataOutput, TFormat}
import de.sciss.synth.proc.Action

import scala.collection.immutable.{IndexedSeq => Vec}

// XXX TODO --- complete DRY with Control
object ActionImpl {
  private final val SER_VERSION = 0x4163  // "Ac"

  def apply[T <: Txn[T]]()(implicit tx: T): Action[T] = new New[T](tx)

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Action[T] =
    format[T].readT(in)

  def format[T <: Txn[T]]: TFormat[T, Action[T]] = anyFmt.asInstanceOf[Fmt[T]]

  private val anyFmt = new Fmt[AnyTxn]

  private class Fmt[T <: Txn[T]] extends ObjFormat[T, Action[T]] {
    def tpe: Obj.Type = Action
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Action[T] = {
    val targets = Targets.read(in)
    val serVer  = in.readShort()
    if (serVer == SER_VERSION) {
      new Read(in, targets, tx)
    } else {
      sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }
  }

  // ---- node impl ----

  private sealed trait Impl[T <: Txn[T]]
    extends Action[T] with SingleEventNode[T, Action.Update[T]] {
    proc =>

    // --- impl ----

    final def tpe: Obj.Type = Action

    override def toString: String = s"Action$id"

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out] = Targets[Out]()
        val graph: Action.GraphObj.Var[Out] = context(proc.graph)

        connect()
      }

    // ---- key maps ----

    final def connect()(implicit tx: T): this.type = {
      graph.changed ---> changed
      this
    }

    private def disconnect()(implicit tx: T): Unit = {
      graph.changed -/-> changed
    }

    object changed extends Changed
      with GeneratorEvent[T, Action.Update[T]] {
      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Action.Update[T]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Action.Update[T]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Action.Change[T]]) { u =>
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

    final protected def disposeData()(implicit tx: T): Unit = {
      disconnect()
      graph.dispose()
    }
  }

  private final class New[T <: Txn[T]](tx0: T) extends Impl[T] {
    protected val targets: Targets[T] = Targets()(tx0)
    val graph: Action.GraphObj.Var[T] = Action.GraphObj.newVar(Action.GraphObj.empty(tx0))(tx0)
    connect()(tx0)
  }

  private final class Read[T <: Txn[T]](in: DataInput, protected val targets: Targets[T], tx0: T)
    extends Impl[T] {

    val graph: Action.GraphObj.Var[T] = Action.GraphObj.readVar(in)(tx0)
  }
}