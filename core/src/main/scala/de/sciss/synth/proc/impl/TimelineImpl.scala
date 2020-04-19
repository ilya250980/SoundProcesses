/*
 *  TimelineImpl.scala
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

import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.bitemp.impl.BiGroupImpl.TreeImpl
import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.Timeline

object TimelineImpl {
  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Timeline.Modifiable[S] =
    new Impl[S](evt.Targets[S]) {
      val tree: TreeImpl[S, Obj] = newTree()
    }

  // ---- serialization ----

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline[S]] =
    anySer.asInstanceOf[Ser[S]]

  implicit def modSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline.Modifiable[S]] =
    anyModSer.asInstanceOf[ModSer[S]]

  //  def modRead[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline.Modifiable[S] = {
  //    val targets = evt.Targets.read[S](in, access)
  //    read(in, access, targets)
  //  }

  private val anySer    = new Ser   [NoSys]
  private val anyModSer = new ModSer[NoSys]

  private class Ser[S <: Sys[S]] extends ObjSerializer[S, Timeline[S]] {
    def tpe: Obj.Type = Timeline
  }

  private class ModSer[S <: Sys[S]] extends ObjSerializer[S, Timeline.Modifiable[S]] {
    def tpe: Obj.Type = Timeline
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline[S] = {
    val targets = Targets.read(in, access)
    new Impl[S](targets) {
      val tree: TreeImpl[S, Obj] = readTree(in,access)
    }
  }

  // ---- impl ----

  private abstract class Impl[S <: Sys[S]](protected val targets: evt.Targets[S])
    extends BiGroupImpl.Impl[S, Obj, Impl[S]] with Timeline.Modifiable[S] { in =>

    // type A = Obj[S]

    override def modifiableOption: Option[Timeline.Modifiable[S]] = Some(this)

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl(Targets[Out]) { out =>
        val tree: TreeImpl[Out, Obj] = newTree()
        context.defer(in, out)(BiGroupImpl.copyTree[S, Out, Obj, Impl[Out]](in.tree, out.tree, out))
        // .connect()
      }

    def tpe: Obj.Type = Timeline

//    def elemSerializer: Serializer[S#Tx, S#Acc, Obj[S]] = Obj.serializer[S]

    override def toString: String = s"Timeline${tree.id}"
  }
}