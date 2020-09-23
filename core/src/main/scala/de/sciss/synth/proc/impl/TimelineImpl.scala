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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.BiGroupImpl.TreeImpl
import de.sciss.lucre.impl.{BiGroupImpl, ObjFormat}
import de.sciss.lucre.{AnyTxn, Copy, Elem, Obj, Txn}
import de.sciss.serial.{DataInput, TFormat}

object TimelineImpl {
  def apply[T <: Txn[T]]()(implicit tx: T): Timeline.Modifiable[T] =
    new Impl[T](Targets[T]()) {
      val tree: TreeImpl[T, Obj] = newTree()
    }

  // ---- serialization ----

  implicit def format[T <: Txn[T]]: TFormat[T, Timeline[T]] =
    anyFmt.asInstanceOf[Fmt[T]]

  implicit def modFormat[T <: Txn[T]]: TFormat[T, Timeline.Modifiable[T]] =
    anyModFmt.asInstanceOf[ModFmt[T]]

  //  def modRead[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): Timeline.Modifiable[T] = {
  //    val targets = evt.Targets.read[T](in, access)
  //    read(in, access, targets)
  //  }

  private val anyFmt    = new Fmt   [AnyTxn]
  private val anyModFmt = new ModFmt[AnyTxn]

  private class Fmt[T <: Txn[T]] extends ObjFormat[T, Timeline[T]] {
    def tpe: Obj.Type = Timeline
  }

  private class ModFmt[T <: Txn[T]] extends ObjFormat[T, Timeline.Modifiable[T]] {
    def tpe: Obj.Type = Timeline
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Timeline[T] = {
    val targets = Targets.read(in)
    new Impl[T](targets) {
      val tree: TreeImpl[T, Obj] = readTree(in)
    }
  }

  // ---- impl ----

  private abstract class Impl[T <: Txn[T]](protected val targets: Targets[T])
    extends BiGroupImpl.Impl[T, Obj, Impl[T]] with Timeline.Modifiable[T] { in =>

    // type A = Obj[T]

    override def modifiableOption: Option[Timeline.Modifiable[T]] = Some(this)

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl(Targets[Out]()) { out =>
        val tree: TreeImpl[Out, Obj] = newTree()
        context.defer(in, out)(BiGroupImpl.copyTree[T, Out, Obj, Impl[Out]](in.tree, out.tree, out))
        // .connect()
      }

    def tpe: Obj.Type = Timeline

//    def elemFormat: Format[T, S#Acc, Obj[T]] = Obj.serializer[T]

    override def toString: String = s"Timeline${tree.id}"
  }
}