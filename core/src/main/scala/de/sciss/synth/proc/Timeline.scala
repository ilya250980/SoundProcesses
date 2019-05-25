/*
 *  Timeline.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.event.EventLike
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{TimelineImpl => Impl}

object Timeline extends Obj.Type {
  final val typeId = 0x10006

  type Update[S <: Sys[S]]          = BiGroup.Update[S, Obj[S], Timeline[S]]
  val  Update: BiGroup.Update.type  = BiGroup.Update

  def apply[S <: Sys[S]]()(implicit tx: S#Tx): Modifiable[S] = Impl[S]

  object Modifiable {
    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] = Impl.modSerializer[S]

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      serializer[S].read(in, access)
  }
  trait Modifiable[S <: Sys[S]] extends Timeline[S] with BiGroup.Modifiable[S, Obj[S]] {
    override def changed: EventLike[S, BiGroup.Update[S, Obj[S], Modifiable[S]]]
  }

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Timeline[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Timeline[S] =
    serializer[S].read(in, access)

  // ---- events ----
  type Added   [S <: Sys[S]] = BiGroup.Added  [S, Obj[S]]
  type Removed [S <: Sys[S]] = BiGroup.Removed[S, Obj[S]]
  type Moved   [S <: Sys[S]] = BiGroup.Moved  [S, Obj[S]]

  val Added  : BiGroup.Added  .type = BiGroup.Added
  val Removed: BiGroup.Removed.type = BiGroup.Removed
  val Moved  : BiGroup.Moved  .type = BiGroup.Moved

  type Timed[S <: Sys[S]] = BiGroup.Entry[S, Obj[S]]
  type Leaf [S <: Sys[S]] = BiGroup.Leaf[S, Obj[S]]

  val Timed : BiGroup.Entry.type = BiGroup.Entry

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait Timeline[S <: Sys[S]] extends BiGroup[S, Obj[S]] {
  override def modifiableOption: Option[Timeline.Modifiable[S]]

  override def changed: EventLike[S, BiGroup.Update[S, Obj[S], Timeline[S]]]
}