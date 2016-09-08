/*
 *  Folder.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.expr
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{FolderImpl => Impl}

import scala.language.existentials

object Folder extends Obj.Type {
  final val typeID = 0x10000

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  def apply[S <: Sys[S]](implicit tx: S#Tx): Folder[S] = Impl[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Folder[S] =
    serializer[S].read(in, access)

  type Update[S <: Sys[S]] = expr.List.Update[S, Obj[S]]

  type Change[S <: Sys[S]]  = expr.List.Change[S, Obj[S]]
  type Added [S <: Sys[S]]  = expr.List.Added[S, Obj[S]]
  val Added                 = expr.List.Added
  type Removed[S <: Sys[S]] = expr.List.Removed[S, Obj[S]]
  val Removed               = expr.List.Removed

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Folder[S]] =
    Impl.serializer[S]
}
trait Folder[S <: Sys[S]] extends expr.List.Modifiable[S, Obj[S]] {
  /** This is simply because we inherit from `expr.List`. We refine the return type here. */
  override def modifiableOption: Option[Folder[S]]
}