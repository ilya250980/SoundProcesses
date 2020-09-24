/*
 *  Cursors.scala
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

import de.sciss.lucre.confluent.{Access, Txn => KTxn}
import de.sciss.lucre.{Elem, EventLike, Publisher, StringObj, Txn, confluent}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.synth.proc.impl.{CursorsImpl => Impl}
import de.sciss.{serial, model => m}

import scala.collection.immutable.{IndexedSeq => Vec}

object Cursors extends Elem.Type {
  final val typeId = 0x1000C

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Elem[T] =
    Impl.readIdentifiedObj(in)

  def apply[T <: KTxn[T], D1 <: Txn[D1]](seminal: Access[T])(implicit tx: D1): Cursors[T, D1] =
    Impl[T, D1](seminal)

  implicit def format[T <: KTxn[T], D1 <: Txn[D1]]: TFormat[D1, Cursors[T, D1]] =
    Impl.format[T, D1]

  final case class Update[T <: KTxn[T], D <: Txn[D]](source: Cursors[T, D], changes: Vec[Change[T, D]])

  sealed trait Change[T <: KTxn[T], D <: Txn[D]]

  final case class Renamed     [T <: KTxn[T], D <: Txn[D]](change: m.Change[String])       extends Change[T, D]
  final case class ChildAdded  [T <: KTxn[T], D <: Txn[D]](idx: Int, child: Cursors[T, D]) extends Change[T, D]
  final case class ChildRemoved[T <: KTxn[T], D <: Txn[D]](idx: Int, child: Cursors[T, D]) extends Change[T, D]
  final case class ChildUpdate [T <: KTxn[T], D <: Txn[D]](change: Update[T, D])           extends Change[T, D]
}
trait Cursors[T <: KTxn[T], D <: Txn[D]]
  extends Elem[D] with Publisher[D, Cursors.Update[T, D]] with serial.Writable {

  def seminal: Access[T]

  def cursor: confluent.Cursor.Data[T, D]

  def name(implicit tx: D): StringObj[D]
  def name_=(value: StringObj[D])(implicit tx: D): Unit

  def descendants(implicit tx: D): Iterator[Cursors[T, D]]

  def addChild(seminal: Access[T])(implicit tx: D): Cursors[T, D]
  def removeChild(child: Cursors[T, D])(implicit tx: D): Unit

  def changed: EventLike[D, Cursors.Update[T, D]]
}