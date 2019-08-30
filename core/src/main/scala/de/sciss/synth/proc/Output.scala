/*
 *  Output.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{Serializer, DataInput}
import impl.{OutputImpl => Impl}

object Output extends Obj.Type {
  final val typeId = 0x10009

  @deprecated("Impl should use quasi-private OutputImpl.apply", "3.4.1")
  def apply[S <: Sys[S]](proc: Proc[S], key: String)(implicit tx: S#Tx): Output[S] = {
    println("WARNING: proc.Output.apply is deprecated.")
    Impl.apply(proc, key)
  }

  def read [S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Output[S] = Impl.read (in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Output[S]] = Impl.serializer

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait Output[S <: Sys[S]] extends Obj[S] {
  def proc: Proc[S]
  def key : String  // or `StringObj`?
}