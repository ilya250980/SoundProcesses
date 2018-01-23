/*
 *  Cursors.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.confluent.{Sys => KSys}
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm.{Elem, Sys, DurableLike => DSys}
import de.sciss.lucre.{confluent, stm, event => evt}
import de.sciss.synth.proc.impl.{CursorsImpl => Impl}
import de.sciss.serial.DataInput
import de.sciss.{serial, model => m}

import scala.collection.immutable.{IndexedSeq => Vec}

object Cursors extends Elem.Type {
  final val typeID = 0x1000C

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] =
    Impl.readIdentifiedObj(in: DataInput, access)

  def apply[S <: KSys[S], D1 <: DSys[D1]](seminal: S#Acc)(implicit tx: D1#Tx): Cursors[S, D1] =
    Impl[S, D1](seminal)

  implicit def serializer[S <: KSys[S], D1 <: DSys[D1]]:
      serial.Serializer[D1#Tx, D1#Acc, Cursors[S, D1]] =
    Impl.serializer[S, D1]

  final case class Update[S <: KSys[S], D <: stm.Sys[D]](source: Cursors[S, D], changes: Vec[Change[S, D]])

  sealed trait Change[S <: KSys[S], D <: stm.Sys[D]]

  final case class Renamed     [S <: KSys[S], D <: stm.Sys[D]](change: m.Change[String])       extends Change[S, D]
  final case class ChildAdded  [S <: KSys[S], D <: stm.Sys[D]](idx: Int, child: Cursors[S, D]) extends Change[S, D]
  final case class ChildRemoved[S <: KSys[S], D <: stm.Sys[D]](idx: Int, child: Cursors[S, D]) extends Change[S, D]
  final case class ChildUpdate [S <: KSys[S], D <: stm.Sys[D]](change: Update[S, D])           extends Change[S, D]
}
trait Cursors[S <: KSys[S], D <: stm.Sys[D]]
  extends Elem[D] with evt.Publisher[D, Cursors.Update[S, D]] with serial.Writable {

  def seminal: S#Acc

  def cursor: confluent.Cursor.Data[S, D]

  def name(implicit tx: D#Tx): StringObj[D]
  def name_=(value: StringObj[D])(implicit tx: D#Tx): Unit

  def descendants(implicit tx: D#Tx): Iterator[Cursors[S, D]]

  def addChild(seminal: S#Acc)(implicit tx: D#Tx): Cursors[S, D]
  def removeChild(child: Cursors[S, D])(implicit tx: D#Tx): Unit

  def changed: evt.EventLike[D, Cursors.Update[S, D]]
}