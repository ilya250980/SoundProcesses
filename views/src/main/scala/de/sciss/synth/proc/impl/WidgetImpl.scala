/*
 *  WidgetImpl.scala
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
import de.sciss.lucre.synth.AnyTxn
import de.sciss.lucre.{Copy, Elem, Obj, Pull, Txn}
import de.sciss.serial.{DataInput, DataOutput, TFormat}
import de.sciss.synth.proc.Widget

import scala.collection.immutable.{IndexedSeq => Vec}

// XXX TODO --- complete DRY with Control
object WidgetImpl {
  private final val SER_VERSION = 0x4674  // "Ft" (why?)

  def apply[T <: Txn[T]]()(implicit tx: T): Widget[T] = new New[T](tx)

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Widget[T] =
    format[T].readT(in)

  def format[T <: Txn[T]]: TFormat[T, Widget[T]] = anyFmt.asInstanceOf[Fmt[T]]

  private val anyFmt = new Fmt[AnyTxn]

  private class Fmt[T <: Txn[T]] extends ObjFormat[T, Widget[T]] {
    def tpe: Obj.Type = Widget
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Widget[T] = {
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
    extends Widget[T] with SingleEventNode[T, Widget.Update[T]] {
    proc =>

    // --- abstract ----

//    protected def outputsMap: SkipList.Map[T, String, Output[T]]

    // --- impl ----

    final def tpe: Obj.Type = Widget

    override def toString: String = s"Widget$id"

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out]                       = Targets[Out]()
        val graph     : Widget.GraphObj.Var[Out]                  = context(proc.graph)
//        val outputsMap: SkipList.Map[Out, String, Output[Out]]  = SkipList.Map.empty

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
      with GeneratorEvent[T, Widget.Update[T]] {
      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Widget.Update[T]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Widget.Update[T]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Widget.Change[T]]) { u =>
          Vector(Widget.GraphChange(u))
        }

        val seq3 = stateOpt.fold(seq0) { u =>
          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Widget.Update(proc, seq3))
      }
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      graph     .write(out)
//      outputsMap.write(out)
    }

    final protected def disposeData()(implicit tx: T): Unit = {
      disconnect()
      graph     .dispose()
//      outputsMap.dispose()
    }
  }

  private final class New[T <: Txn[T]](tx0: T) extends Impl[T] {
    protected val targets: Targets[T] = Targets()(tx0)
    val graph     : Widget.GraphObj.Var[T]                 = Widget.GraphObj.newVar(Widget.GraphObj.empty(tx0))(tx0)
//    val outputsMap: SkipList.Map[T, String, Output[T]]  = SkipList.Map.empty
    connect()(tx0)
  }

  private final class Read[T <: Txn[T]](in: DataInput, protected val targets: Targets[T], tx0: T)
    extends Impl[T] {

    val graph     : Widget.GraphObj.Var[T]                 = Widget.GraphObj.readVar(in)(tx0)
//    val outputsMap: SkipList.Map[T, String, Output[T]]  = SkipList.Map.read   (in, access)
  }
}