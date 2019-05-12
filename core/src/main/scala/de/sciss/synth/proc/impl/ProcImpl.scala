/*
 *  ProcImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.{Output, Outputs, Proc, SynthGraphObj}

import scala.collection.immutable.{IndexedSeq => Vec}

object ProcImpl {
  private final val SER_VERSION = 0x5074  // was "Pr"

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Proc[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Proc[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Proc[S]] {
    def tpe: Obj.Type = Proc
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Proc[S] = {
    val targets = Targets.read(in, access)
    new Read(in, access, targets)
  }

//  private type I = InMemory

  final class OutputsImpl[S <: Sys[S]](proc: Impl[S], val slot: Int, isInput: Boolean)
    extends Outputs[S] {

    // ---- key-map-impl details ----

    import proc.outputsMap

    protected def fire(added: Option[Output[S]], removed: Option[Output[S]])
                      (implicit tx: S#Tx): Unit = {
      val b = Vector.newBuilder[Proc.OutputsChange[S]]
      b.sizeHint(2)
      // convention: first the removals, then the additions. thus, overwriting a key yields
      // successive removal and addition of the same key.
      removed.foreach { output =>
        b += Proc.OutputRemoved[S](output)
      }
      added.foreach { output =>
        b += Proc.OutputAdded  [S](output)
      }

      proc.changed.fire(Proc.Update(proc, b.result()))
    }

    private def add(key: String, value: Output[S])(implicit tx: S#Tx): Unit = {
      val optRemoved = outputsMap.put(key, value)
      fire(added = Some(value), removed = optRemoved)
    }

    def remove(key: String)(implicit tx: S#Tx): Boolean =
      outputsMap.remove(key).exists { output =>
        fire(added = None, removed = Some(output))
        true
      }

    def add(key: String)(implicit tx: S#Tx): Output[S] =
      get(key).getOrElse {
        val res = OutputImpl[S](proc, key)
        add(key, res)
        res
      }

    def get(key: String)(implicit tx: S#Tx): Option[Output[S]] = outputsMap.get(key)

    def keys(implicit tx: S#Tx): Set[String] = outputsMap.keysIterator.toSet

    def iterator(implicit tx: S#Tx): Iterator[Output[S]] = outputsMap.iterator.map(_._2)
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Proc[S] with evt.impl.SingleNode[S, Proc.Update[S]] {
    proc =>

    final def tpe: Obj.Type = Proc

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out]                     = Targets[Out]
        val graph     : SynthGraphObj.Var[Out]                  = context(proc.graph)
        val outputsMap: SkipList.Map[Out, String, Output[Out]]  = SkipList.Map.empty

        context.defer(proc, out) {
          def copyMap(in : SkipList.Map[S  , String, Output[S  ]],
                      out: SkipList.Map[Out, String, Output[Out]]): Unit =
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

    // def scanInMap : SkipList.Map[S, String, ScanEntry[S]]
    def outputsMap: SkipList.Map[S, String, Output[S]]

    // ---- key maps ----

//    def isConnected(implicit tx: S#Tx): Boolean = targets.nonEmpty

//    sealed trait ProcEvent {
//      final def node: Proc[S] with evt.Node[S] = proc
//    }

//    final val inputs  = new ScansImpl(this, 1, isInput = true )
    final val outputs = new OutputsImpl(this, 2, isInput = false)

//    object StateEvent
//      extends evti.TriggerImpl[S, Proc.Update[S], Proc[S]]
//      with evti.Root          [S, Proc.Update[S]]
//      with ProcEvent {
//
//      final val slot = 3
//    }

    final def connect()(implicit tx: S#Tx): this.type = {
      graph.changed ---> changed
      // inputs        ---> changed
      // outputs       ---> changed
      // StateEvent    ---> this
      this
    }

    private def disconnect()(implicit tx: S#Tx): Unit = {
      graph.changed -/-> changed
      // inputs        -/-> changed
      // outputs       -/-> changed
      // StateEvent    -/-> this
    }

    object changed extends Changed
      with evt.impl.Generator[S, Proc.Update[S]]
      // with evt.impl.Root[S, Proc.Update[S]]
      // extends evt.impl.EventImpl[S, Proc.Update[S], Proc[S]]
       {

      // final val slot = 4

      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Proc.Update[S]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Proc.Update[S]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Change[S]]) { u =>
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

//    final def event(slot: Int /*, invariant: Boolean */): Event[S, Any] = (slot: @switch) match {
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

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      graph       .dispose()
      // scanInMap   .dispose()
      outputsMap  .dispose()
    }

    override def toString: String = s"Proc$id"
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets: Targets[S] = evt.Targets[S](tx0)
    val graph     : SynthGraphObj.Var[S]                = SynthGraphObj.newVar(SynthGraphObj.empty)
    val outputsMap: SkipList.Map[S, String, Output[S]]  = SkipList.Map.empty
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: evt.Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    {
      val serVer = in.readShort()
      if (serVer != SER_VERSION) sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }

    val graph     : SynthGraphObj.Var[S]                = SynthGraphObj.readVar(in, access)
    val outputsMap: SkipList.Map[S, String, Output[S]]  = SkipList.Map .read   (in, access)
  }
}