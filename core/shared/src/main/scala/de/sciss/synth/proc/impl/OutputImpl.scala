/*
 *  OutputImpl.scala
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

import de.sciss.lucre.impl.{ConstObjImpl, ObjCastFormat}
import de.sciss.lucre.{AnyTxn, Copy, Elem, Ident, Obj, Txn}
import de.sciss.serial.{DataInput, DataOutput, TFormat}

object OutputImpl {
  private final val SER_VERSION = 0x5370  // was "Sn"

  sealed trait Update[T]

  def apply[T <: Txn[T]](proc: Proc[T], key: String)(implicit tx: T): Proc.Output[T] = {
    val id = tx.newId()
    new Impl(id, proc, key)
  }

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Proc.Output[T] =
    format[T].readT(in)

  def format[T <: Txn[T]]: TFormat[T, Proc.Output[T]] = anyFmt.cast

  private val anyFmt = new Fmt[AnyTxn]

  private final class Fmt[T <: Txn[T]] extends ObjCastFormat[T, Proc.Output] {
    def tpe: Obj.Type = Proc.Output
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Proc.Output[T] = {
    val cookie  = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, expected 3 found $cookie")
    val id      = tx.readId(in)
    val serVer  = in.readShort()
    if (serVer != SER_VERSION)
      sys.error(s"Incompatible serialized version (found ${serVer.toInt.toHexString}, required ${SER_VERSION.toHexString})")

    val proc  = Proc.read(in)
    val key   = in.readUTF()
    new Impl(id, proc, key)
  }

  // private final val filterAll: Any => Boolean = _ => true

  private final class Impl[T <: Txn[T]](val id: Ident[T], val proc: Proc[T], val key: String)
    extends Proc.Output[T] with ConstObjImpl[T, Any] {

    def tpe: Obj.Type = Proc.Output

    override def toString: String = s"Output($id, $proc, $key)"

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] = {
      val out = new Impl(txOut.newId(), context(proc), key)
      out // .connect()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      proc.write(out)
      out.writeUTF(key)
    }
  }
}