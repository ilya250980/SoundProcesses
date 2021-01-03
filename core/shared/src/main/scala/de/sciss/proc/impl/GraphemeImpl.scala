/*
 *  GraphemeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc.impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.BiPinImpl.Tree
import de.sciss.lucre.impl.{BiPinImpl, ObjCastFormat}
import de.sciss.lucre.{AnyTxn, Copy, Elem, Obj, Txn}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.proc.Grapheme

object GraphemeImpl {
  import de.sciss.proc.Grapheme.Modifiable

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Grapheme[T] =
    format[T].readT(in)

  def readModifiable[T <: Txn[T]](in: DataInput)(implicit tx: T): Grapheme.Modifiable[T] = {
    modifiableFormat[T].readT(in)
  }

  implicit def format[T <: Txn[T]]: TFormat[T, Grapheme[T]] = anyFmt.cast

  implicit def modifiableFormat[T <: Txn[T]]: TFormat[T, Grapheme.Modifiable[T]] = anyModFmt.cast

  private val anyFmt    = new Fmt[AnyTxn]
  private val anyModFmt = new ModFmt[AnyTxn]

  private final class Fmt[T <: Txn[T]] extends ObjCastFormat[T, Grapheme] {
    def tpe: Obj.Type = Grapheme
  }

  private final class ModFmt[T <: Txn[T]] extends ObjCastFormat[T, Grapheme.Modifiable] {
    def tpe: Obj.Type = Grapheme
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Grapheme[T] = {
    val targets = Targets.read(in)
    new Impl(targets) {
      val tree: Tree[T, Obj[T]] = readTree(in)
    }
  }

  def modifiable[T <: Txn[T]](implicit tx: T): Modifiable[T] = {
    val targets = Targets[T]()
    new Impl[T](targets) {
      val tree: Tree[T, Obj[T]] = newTree()
    } // .connect()
  }

  // ---- actual implementation ----

  private abstract class Impl[T <: Txn[T]](protected val targets: Targets[T])
    extends BiPinImpl.Impl[T, Obj, Impl[T]] with Grapheme.Modifiable[T] {
    in =>

    final def tpe: Obj.Type = Grapheme

    override def toString: String = s"Grapheme$id"

    final def modifiableOption: Option[Modifiable[T]] = Some(this)

    final def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl[Out](Targets[Out]()) { out =>
        val tree: Tree[Out, A] = out.newTree()
        context.defer[PinAux](in, out)(BiPinImpl.copyTree[T, Out, Obj, Impl[Out]](in.tree, out.tree, out))
        // out.connect()
      }

    final def firstEvent(implicit tx: T): Option[Long] = eventAfter  (Long.MinValue)
    final def lastEvent (implicit tx: T): Option[Long] = eventBefore (Long.MaxValue)
  }
}