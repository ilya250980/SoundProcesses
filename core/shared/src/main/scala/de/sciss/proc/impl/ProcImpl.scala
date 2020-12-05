/*
 *  ProcImpl.scala
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

package de.sciss.proc
package impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.data.SkipList
import de.sciss.lucre.impl.{GeneratorEvent, ObjCastFormat, SingleEventNode}
import de.sciss.lucre.{AnyTxn, Copy, Elem, Obj, Pull, Txn}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

import scala.collection.immutable.{IndexedSeq => Vec}

object ProcImpl {
  private final val SER_VERSION = 0x5074  // was "Pr"

  def apply[T <: Txn[T]]()(implicit tx: T): Proc[T] = new New[T]

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Proc[T] =
    format[T].readT(in)

  def format[T <: Txn[T]]: TFormat[T, Proc[T]] = anyFmt.cast

  private val anyFmt = new Mod[AnyTxn]

  private class Mod[T <: Txn[T]] extends ObjCastFormat[T, Proc] {
    def tpe: Obj.Type = Proc
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Proc[T] = {
    val targets = Targets.read(in)
    new Read(in, targets)
  }

//  private type I = InMemory

  private final class OutputsImpl[T <: Txn[T]](proc: Impl[T], val slot: Int, isInput: Boolean)
    extends Proc.Outputs[T] {

    // ---- key-map-impl details ----

    import proc.outputsMap

    protected def fire(added: Option[Proc.Output[T]], removed: Option[Proc.Output[T]])
                      (implicit tx: T): Unit = {
      val b = Vector.newBuilder[Proc.OutputsChange[T]]
      b.sizeHint(2)
      // convention: first the removals, then the additions. thus, overwriting a key yields
      // successive removal and addition of the same key.
      removed.foreach { output =>
        b += Proc.OutputRemoved[T](output)
      }
      added.foreach { output =>
        b += Proc.OutputAdded  [T](output)
      }

      proc.changed.fire(Proc.Update(proc, b.result()))
    }

    private def add(key: String, value: Proc.Output[T])(implicit tx: T): Unit = {
      val optRemoved = outputsMap.put(key, value)
      fire(added = Some(value), removed = optRemoved)
    }

    def remove(key: String)(implicit tx: T): Boolean =
      outputsMap.remove(key).exists { output =>
        fire(added = None, removed = Some(output))
        true
      }

    def add(key: String)(implicit tx: T): Proc.Output[T] =
      get(key).getOrElse {
        val res = ProcOutputImpl[T](proc, key)
        add(key, res)
        res
      }

    def get(key: String)(implicit tx: T): Option[Proc.Output[T]] = outputsMap.get(key)

    def keys(implicit tx: T): Set[String] = outputsMap.keysIterator.toSet

    def iterator(implicit tx: T): Iterator[Proc.Output[T]] = outputsMap.iterator.map(_._2)
  }

  private sealed trait Impl[T <: Txn[T]]
    extends Proc[T] with SingleEventNode[T, Proc.Update[T]] {
    proc =>

    final def tpe: Obj.Type = Proc

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out]                         = Targets[Out]()
        val graph     : Proc.GraphObj.Var[Out]                      = context(proc.graph)
        val outputsMap: SkipList.Map[Out, String, Proc.Output[Out]] = SkipList.Map.empty

        context.defer(proc, out) {
          def copyMap(in : SkipList.Map[T  , String, Proc.Output[T  ]],
                      out: SkipList.Map[Out, String, Proc.Output[Out]]): Unit =
          in.iterator.foreach { case (key, eIn) =>
            val eOut = context(eIn)
            out.put(key, eOut)
          }

          // copyMap(proc.scanInMap , out.scanInMap)
          copyMap(proc.outputsMap, out.outputsMap)
        }
        connect()
      }

    import Proc._

    // def scanInMap : SkipList.Map[T, String, ScanEntry[T]]
    def outputsMap: SkipList.Map[T, String, Output[T]]

    // ---- key maps ----

//    def isConnected(implicit tx: T): Boolean = targets.nonEmpty

//    sealed trait ProcEvent {
//      final def node: Proc[T] with evt.Node[T] = proc
//    }

//    final val inputs  = new ScansImpl(this, 1, isInput = true )
    final val outputs = new OutputsImpl(this, 2, isInput = false)

//    object StateEvent
//      extends evti.TriggerImpl[T, Proc.Update[T], Proc[T]]
//      with evti.Root          [T, Proc.Update[T]]
//      with ProcEvent {
//
//      final val slot = 3
//    }

    final def connect()(implicit tx: T): this.type = {
      graph.changed ---> changed
      // inputs        ---> changed
      // outputs       ---> changed
      // StateEvent    ---> this
      this
    }

    private def disconnect()(implicit tx: T): Unit = {
      graph.changed -/-> changed
      // inputs        -/-> changed
      // outputs       -/-> changed
      // StateEvent    -/-> this
    }

    object changed extends Changed
      with GeneratorEvent[T, Proc.Update[T]]
      // with evt.impl.Root[T, Proc.Update[T]]
      // extends evt.impl.EventImpl[T, Proc.Update[T], Proc[T]]
       {

      // final val slot = 4

      def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Proc.Update[T]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Proc.Update[T]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Change[T]]) { u =>
          Vector(GraphChange(u))
        }
//        val seq1 = seq0
//          scanInsOpt.fold(seq0) { u =>
//          if (seq0.isEmpty) u.changes else seq0 ++ u.changes
//        }
//        val seq2 = seq1
//        scanOutsOpt.fold(seq1) { u =>
//          if (seq1.isEmpty) u.changes else seq1 ++ u.changes
//        }
        val seq3 = stateOpt.fold(seq0 /* seq2 */) { u =>
          if (seq0 /* seq2 */.isEmpty) u.changes else seq0 /* seq2 */ ++ u.changes
        }
        if (seq3.isEmpty) None else Some(Proc.Update(proc, seq3))
      }
    }

//    final def event(slot: Int /*, invariant: Boolean */): Event[T, Any] = (slot: @switch) match {
//      case ChangeEvent.slot => ChangeEvent
//      case 1 /* inputs .slot */ => inputs
//      case 2 /* outputs.slot */ => outputs
//      case StateEvent .slot => StateEvent
//    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      graph       .write(out)
      // scanInMap   .write(out)
      outputsMap  .write(out)
    }

    final protected def disposeData()(implicit tx: T): Unit = {
      disconnect()
      graph       .dispose()
      // scanInMap   .dispose()
      outputsMap  .dispose()
    }

    override def toString: String = s"Proc$id"
  }

  private final class New[T <: Txn[T]](implicit tx0: T) extends Impl[T] {
    protected val targets: Targets[T] = Targets[T]()(tx0)
    val graph     : Proc.GraphObj.Var[T]                    = Proc.GraphObj.newVar(Proc.GraphObj.empty)
    val outputsMap: SkipList.Map[T, String, Proc.Output[T]] = SkipList.Map.empty
    connect()(tx0)
  }

  private final class Read[T <: Txn[T]](in: DataInput, protected val targets: Targets[T])
                                       (implicit tx0: T)
    extends Impl[T] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val graph     : Proc.GraphObj.Var[T]                    = Proc.GraphObj.readVar(in)
    val outputsMap: SkipList.Map[T, String, Proc.Output[T]] = SkipList.Map .read   (in)
  }
}