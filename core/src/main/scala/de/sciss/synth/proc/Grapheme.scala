/*
 *  Grapheme.scala
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

import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{GraphemeImpl => Impl}

object Grapheme extends Obj.Type {
  final val typeID = 0x10002

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme[S]] = Impl.serializer[S]

  trait Modifiable[S <: Sys[S]] extends Grapheme[S] with BiPin.Modifiable[S, Obj[S]]

  def apply[S <: Sys[S]](implicit tx: S#Tx): Modifiable[S] = Impl.modifiable[S]

  object Modifiable {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readModifiable(in, access)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] =
      Impl.modifiableSerializer[S]

    /** Extractor to check if a `Grapheme` is actually a `Grapheme.Modifiable`. */
    def unapply[S <: Sys[S]](g: Grapheme[S]): Option[Modifiable[S]] = {
      if (g.isInstanceOf[Modifiable[_]]) Some(g.asInstanceOf[Modifiable[S]]) else None
    }
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = Impl.read(in, access)

  override def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  // ---- types from BiPin ----

  type Leaf   [S <: Sys[S]] = BiPin.Leaf   [S, Obj[S]]
  type Entry  [S <: Sys[S]] = BiPin.Entry  [S, Obj[S]]

  type Added  [S <: Sys[S]] = BiPin.Added  [S, Obj[S]]
  val  Added                = BiPin.Added
  type Removed[S <: Sys[S]] = BiPin.Removed[S, Obj[S]]
  val  Removed              = BiPin.Removed
  type Moved  [S <: Sys[S]] = BiPin.Moved  [S, Obj[S]]
  val  Moved                = BiPin.Moved
}
trait Grapheme[S <: Sys[S]] extends BiPin[S, Obj[S]] {
  import Grapheme.Modifiable

  override def modifiableOption: Option[Modifiable[S]]

  def firstEvent(implicit tx: S#Tx): Option[Long]
}