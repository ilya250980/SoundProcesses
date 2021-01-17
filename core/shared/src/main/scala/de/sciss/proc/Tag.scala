/*
 *  Tag.scala
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

package de.sciss.proc

import de.sciss.lucre.impl.{ConstObjImpl, ObjCastFormat}
import de.sciss.lucre.{AnyTxn, Copy, Elem, Ident, Obj, Txn}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

object Tag extends Obj.Type {
  final val typeId = 31

  def apply[T <: Txn[T]]()(implicit tx: T): Tag[T] = {
    val id = tx.newId()
    new Impl(id)
  }

  implicit def format[T <: Txn[T]]: TFormat[T, Tag[T]] = anyFmt.cast

  private val anyFmt = new Fmt[AnyTxn]

  private final class Fmt[T <: Txn[T]] extends ObjCastFormat[T, Tag] {
    def tpe: Obj.Type = Tag
  }

  private final val SER_VERSION = 0x5467  // "Tg"

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] = {
    val cookie  = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, expected 3 found $cookie")
    val id      = tx.readId(in)
    val serVer  = in.readShort()
    if (serVer != SER_VERSION)
      sys.error(s"Incompatible serialized version (found ${serVer.toInt.toHexString}, required ${SER_VERSION.toHexString})")
    new Impl(id)
  }

  private[proc] final class Impl[T <: Txn[T]](val id: Ident[T]) extends Tag[T] with ConstObjImpl[T, Any] {
    override def tpe: Obj.Type = Tag

    protected def writeData(out: DataOutput): Unit =
      out.writeShort(SER_VERSION)

    def copy[Out <: Txn[Out]]()(implicit txIn: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Impl(txOut.newId())
  }
}
/** A tag is an empty object used solely as placeholder and for gathering elements in its attribute map. */
trait Tag[T <: Txn[T]] extends Obj[T]
