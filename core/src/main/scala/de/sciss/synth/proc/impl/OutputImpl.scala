/*
 *  OutputImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.impl.ConstObjImpl
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Elem, NoSys, Obj, Sys}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc.{Output, Proc}

object OutputImpl {
  private final val SER_VERSION = 0x5370  // was "Sn"

  sealed trait Update[S]

  def apply[S <: Sys[S]](proc: Proc[S], key: String)(implicit tx: S#Tx): Output[S] = {
    val id = tx.newId()
    new Impl(id, proc, key)
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Output[S] =
    serializer[S].read(in, access)

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Output[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[NoSys]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Output[S]] {
    def tpe: Obj.Type = Output
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Output[S] = {
    val cookie  = in.readByte()
    if (cookie != 3) sys.error(s"Unexpected cookie, expected 3 found $cookie")
    val id      = tx.readId(in, access)
    val serVer  = in.readShort()
    if (serVer != SER_VERSION)
      sys.error(s"Incompatible serialized version (found ${serVer.toInt.toHexString}, required ${SER_VERSION.toHexString})")

    val proc  = Proc.read(in, access)
    val key   = in.readUTF()
    new Impl(id, proc, key)
  }

  // private final val filterAll: Any => Boolean = _ => true

  private final class Impl[S <: Sys[S]](val id: S#Id, val proc: Proc[S], val key: String)
    extends Output[S] with ConstObjImpl[S, Any] {

    def tpe: Obj.Type = Output

    override def toString: String = s"Output($id, $proc, $key)"

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
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