/*
 *  Timeline.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{TimelineImpl => Impl}

object Timeline extends Obj.Type {
  final val typeID = 0x10006

  type Update[S <: Sys[S]]  = BiGroup.Update[S, Obj[S]]
  val  Update               = BiGroup.Update

  def apply[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl[S]

  object Modifiable {
    // def apply[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl[S]

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer[S]

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      serializer[S].read(in, access)
  }
  trait Modifiable[S <: Sys[S]] extends Timeline[S] with BiGroup.Modifiable[S, Obj[S]]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline[S] =
    serializer[S].read(in, access)

  // ---- events ----
  val Added     = BiGroup.Added
  val Removed   = BiGroup.Removed
  val Moved     = BiGroup.Moved
  // val Element   = BiGroup.ElementMutated

  type Timed[S <: Sys[S]] = BiGroup.Entry[S, Obj[S]]
  type Leaf [S <: Sys[S]] = BiGroup.Leaf[S, Obj[S]]

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait Timeline[S <: Sys[S]] extends BiGroup[S, Obj[S]] {
  override def modifiableOption: Option[Timeline.Modifiable[S]]
}