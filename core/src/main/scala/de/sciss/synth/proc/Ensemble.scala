/*
 *  Ensemble.scala
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

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.{BooleanObj, LongObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Folder, Obj, Sys}
import de.sciss.model
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{EnsembleImpl => Impl}

object Ensemble extends Obj.Type {
  final val typeId = 0x10007

  def apply[S <: Sys[S]](folder: stm.Folder /* Elem.Obj */[S], offset: LongObj[S], playing: BooleanObj[S])
                        (implicit tx: S#Tx): Ensemble[S] = Impl(folder, offset, playing)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Ensemble[S] =
    Impl.serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Ensemble[S]] = Impl.serializer[S]

  final case class Update[S <: Sys[S]](ensemble: Ensemble[S], changes: List[Change[S]])

  sealed trait Change[S]
  final case class Folder [S <: Sys[S]](peer: stm.List.Update[S, Obj[S]] /* SCALAC BUG: proc.Folder.Update[S] */) extends Change[S]
  final case class Offset [S <: Sys[S]](peer: model.Change[Long   ]) extends Change[S]
  final case class Playing[S <: Sys[S]](peer: model.Change[Boolean]) extends Change[S]

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}

/** An `Ensemble` is sort of a persistent transport model.
  * It maintains a list of transported objects through the
  * `folder` member. The `playing` expression determines
  * when the transport is playing or not. Upon a transition
  * from stopped to playing, the `offset` member determines
  * the "seek" position.
  */
trait Ensemble[S <: Sys[S]] extends Obj[S] with Publisher[S, Ensemble.Update[S]] {
  def folder (implicit tx: S#Tx): Folder /* Elem.Obj */ [S]
  def offset (implicit tx: S#Tx): LongObj[S]
  def playing(implicit tx: S#Tx): BooleanObj[S]
}
