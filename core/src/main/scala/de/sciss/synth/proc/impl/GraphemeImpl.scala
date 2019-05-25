/*
 *  GraphemeImpl.scala
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

import de.sciss.lucre.bitemp.impl.BiPinImpl
import de.sciss.lucre.bitemp.impl.BiPinImpl.Tree
import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.lucre.{event => evt}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.Grapheme

object GraphemeImpl {
  import de.sciss.synth.proc.Grapheme.Modifiable

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] =
    serializer[S].read(in, access)

  def readModifiable[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme.Modifiable[S] = {
    modifiableSerializer[S].read(in, access)
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme[S]] =
    anySer.asInstanceOf[Ser[S]]

  implicit def modifiableSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme.Modifiable[S]] =
    anySer.asInstanceOf[Serializer[S#Tx, S#Acc, Grapheme.Modifiable[S]]] // whatever... right now it is modifiable

  private val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Grapheme[S]] {
    def tpe: Obj.Type = Grapheme
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = {
    val targets = Targets.read(in, access)
    new Impl(targets) {
      val tree: Tree[S, Obj[S]] = readTree(in, access)
    }
  }

  def modifiable[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = {
    val targets = evt.Targets[S]
    new Impl[S](targets) {
      val tree: Tree[S, Obj[S]] = newTree()
    } // .connect()
  }

  // ---- actual implementation ----

  private abstract class Impl[S <: Sys[S]](protected val targets: evt.Targets[S])
    extends BiPinImpl.Impl[S, Obj, Impl[S]] with Grapheme.Modifiable[S] {
    in =>

    final def tpe: Obj.Type = Grapheme

    override def toString: String = s"Grapheme$id"

    final def modifiableOption: Option[Modifiable[S]] = Some(this)

    final def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Impl[Out](evt.Targets[Out]) { out =>
        val tree: Tree[Out, A] = out.newTree()
        context.defer[PinAux](in, out)(BiPinImpl.copyTree[S, Out, Obj, Impl[Out]](in.tree, out.tree, out))
        // out.connect()
      }

    final def firstEvent(implicit tx: S#Tx): Option[Long] = eventAfter  (Long.MinValue)
    final def lastEvent (implicit tx: S#Tx): Option[Long] = eventBefore (Long.MaxValue)
  }
}