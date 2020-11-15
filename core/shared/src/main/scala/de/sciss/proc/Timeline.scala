/*
 *  Timeline.scala
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

package de.sciss.proc

import de.sciss.lucre.{BiGroup, EventLike, Obj, Txn}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.proc.impl.{TimelineImpl => Impl}

object Timeline extends Obj.Type {
  final val typeId = 0x10006

  type Update[T <: Txn[T]]          = BiGroup.Update[T, Obj[T], Timeline[T]]
  val  Update: BiGroup.Update.type  = BiGroup.Update

  def apply[T <: Txn[T]]()(implicit tx: T): Modifiable[T] = Impl[T]()

  object Modifiable {
    implicit def format[T <: Txn[T]]: TFormat[T, Modifiable[T]] = Impl.modFormat[T]

    def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Modifiable[T] =
      format[T].readT(in)
  }
  trait Modifiable[T <: Txn[T]] extends Timeline[T] with BiGroup.Modifiable[T, Obj[T]] {
    override def changed: EventLike[T, BiGroup.Update[T, Obj[T], Modifiable[T]]]
  }

  implicit def format[T <: Txn[T]]: TFormat[T, Timeline[T]] = Impl.format[T]

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Timeline[T] =
    format[T].readT(in)

  // ---- events ----
  type Added   [T <: Txn[T]] = BiGroup.Added  [T, Obj[T]]
  type Removed [T <: Txn[T]] = BiGroup.Removed[T, Obj[T]]
  type Moved   [T <: Txn[T]] = BiGroup.Moved  [T, Obj[T]]

  val Added  : BiGroup.Added  .type = BiGroup.Added
  val Removed: BiGroup.Removed.type = BiGroup.Removed
  val Moved  : BiGroup.Moved  .type = BiGroup.Moved

  type Timed[T <: Txn[T]] = BiGroup.Entry[T, Obj[T]]
  type Leaf [T <: Txn[T]] = BiGroup.Leaf[T, Obj[T]]

  val Timed : BiGroup.Entry.type = BiGroup.Entry

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)
}
trait Timeline[T <: Txn[T]] extends BiGroup[T, Obj[T]] {
  override def modifiableOption: Option[Timeline.Modifiable[T]]

  override def changed: EventLike[T, BiGroup.Update[T, Obj[T], Timeline[T]]]
}