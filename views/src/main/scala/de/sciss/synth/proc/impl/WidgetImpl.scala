/*
 *  WidgetImpl.scala
 *  (Widget)
 *
 *  Copyright (c) 2001-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.swing.{Graph, Widget => _Widget}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object WidgetImpl {
  private final val SER_VERSION = 0x4674

  def apply[S <: Sys[S]](implicit tx: S#Tx): Widget[S] = new New[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Widget[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Widget[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Widget[S]] {
    def tpe: Obj.Type = Widget
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Widget[S] = {
    val targets = Targets.read(in, access)
    val serVer  = in.readShort()
    if (serVer == SER_VERSION) {
      new Read(in, access, targets)
    } else {
      sys.error(s"Incompatible serialized (found $serVer, required $SER_VERSION)")
    }
  }

  // ---- Code ----

  implicit object CodeWrapper extends CodeImpl.Wrapper[Unit, Graph, _Widget, Widget.Code] {
    def id      : Int             = Widget.Code.id
    def binding : Option[String]  = None

    def wrap(in: Unit)(fun: => _Widget): Graph = Graph(fun)

    def blockTag = "de.sciss.lucre.swing.Widget"
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Widget[S] with evt.impl.SingleNode[S, Widget.Update[S]] {
    proc =>

    // --- abstract ----

//    protected def outputsMap: SkipList.Map[S, String, Output[S]]

    // --- impl ----

    final def tpe: Obj.Type = Widget

    override def toString: String = s"Widget$id"

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out] { out =>
        protected val targets: Targets[Out]                     = Targets[Out]
        val graph     : Widget.GraphObj.Var[Out]                   = context(proc.graph)
//        val outputsMap: SkipList.Map[Out, String, Output[Out]]  = SkipList.Map.empty

        connect()
      }

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
      with evt.impl.Generator[S, Widget.Update[S]] {
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Widget.Update[S]] = {
        val graphCh     = graph.changed
        val graphOpt    = if (pull.contains(graphCh)) pull(graphCh) else None
        val stateOpt    = if (pull.isOrigin(this)) Some(pull.resolve[Widget.Update[S]]) else None

        val seq0 = graphOpt.fold(Vec.empty[Widget.Change[S]]) { u =>
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

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      disconnect()
      graph     .dispose()
//      outputsMap.dispose()
    }
  }

  private final class New[S <: Sys[S]](implicit tx0: S#Tx) extends Impl[S] {
    protected val targets: Targets[S] = Targets(tx0)
    val graph     : Widget.GraphObj.Var[S]                 = Widget.GraphObj.newVar(Widget.GraphObj.empty)
//    val outputsMap: SkipList.Map[S, String, Output[S]]  = SkipList.Map.empty
    connect()(tx0)
  }

  private final class Read[S <: Sys[S]](in: DataInput, access: S#Acc, protected val targets: Targets[S])
                                       (implicit tx0: S#Tx)
    extends Impl[S] {

    val graph     : Widget.GraphObj.Var[S]                 = Widget.GraphObj.readVar(in, access)
//    val outputsMap: SkipList.Map[S, String, Output[S]]  = SkipList.Map.read   (in, access)
  }
}