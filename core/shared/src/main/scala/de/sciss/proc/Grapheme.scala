/*
 *  Grapheme.scala
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

import de.sciss.lucre.{BiPin, EventLike, Obj, Txn}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.proc.impl.{GraphemeImpl => Impl}

object Grapheme extends Obj.Type {
  final val typeId = 0x10002

  implicit def format[T <: Txn[T]]: TFormat[T, Grapheme[T]] = Impl.format[T]

  trait Modifiable[T <: Txn[T]] extends Grapheme[T] with BiPin.Modifiable[T, Obj[T]] {
    override def changed: EventLike[T, BiPin.Update[T, Obj[T], Modifiable[T]]]
  }

  def apply[T <: Txn[T]]()(implicit tx: T): Modifiable[T] = Impl.modifiable[T]

  object Modifiable {
    def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Modifiable[T] = Impl.readModifiable(in)

    implicit def format[T <: Txn[T]]: TFormat[T, Modifiable[T]] = Impl.modifiableFormat[T]

    /** Extractor to check if a `Grapheme` is actually a `Grapheme.Modifiable`. */
    def unapply[T <: Txn[T]](g: Grapheme[T]): Option[Modifiable[T]] = {
      if (g.isInstanceOf[Modifiable[_]]) Some(g.asInstanceOf[Modifiable[T]]) else None
    }
  }

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Grapheme[T] = Impl.read(in)

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

  // ---- types from BiPin ----

  type Leaf   [T <: Txn[T]]         = BiPin.Leaf   [T, Obj[T]]
  type Entry  [T <: Txn[T]]         = BiPin.Entry  [T, Obj[T]]
  val  Entry: BiPin.Entry.type      = BiPin.Entry

  type Added  [T <: Txn[T]]         = BiPin.Added  [T, Obj[T]]
  val  Added: BiPin.Added.type      = BiPin.Added
  type Removed[T <: Txn[T]]         = BiPin.Removed[T, Obj[T]]
  val  Removed: BiPin.Removed.type  = BiPin.Removed
  type Moved  [T <: Txn[T]]         = BiPin.Moved  [T, Obj[T]]
  val  Moved: BiPin.Moved.type      = BiPin.Moved
}
trait Grapheme[T <: Txn[T]] extends BiPin[T, Obj[T]] {
  import Grapheme.Modifiable

  override def modifiableOption: Option[Modifiable[T]]

  def firstEvent(implicit tx: T): Option[Long]
  def lastEvent (implicit tx: T): Option[Long]

  override def changed: EventLike[T, BiPin.Update[T, Obj[T], Grapheme[T]]]
}