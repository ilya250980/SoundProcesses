///*
// *  Ensemble.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//
//import de.sciss.lucre.event.Publisher
//import de.sciss.lucre.expr.{BooleanObj, LongObj}
//import de.sciss.lucre.stm
//import de.sciss.lucre.stm.{Folder, Obj, Sys}
//import de.sciss.model
//import de.sciss.serial.{DataInput, TFormat}
//import de.sciss.synth.proc.impl.{EnsembleImpl => Impl}
//
//@deprecated("Should only use Folder now with Control/Action", since = "3.35.3")
//object Ensemble extends Obj.Type {
//  final val typeId = 0x10007
//
//  def apply[T <: Txn[T]](folder: stm.Folder /* Elem.Obj */[T], offset: LongObj[T], playing: BooleanObj[T])
//                        (implicit tx: T): Ensemble[T] = Impl(folder, offset, playing)
//
//  def read[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): Ensemble[T] =
//    Impl.format[T].read(in, access)
//
//  implicit def format[T <: Txn[T]]: TFormat[T, Ensemble[T]] = Impl.format[T]
//
//  final case class Update[T <: Txn[T]](ensemble: Ensemble[T], changes: List[Change[T]])
//
//  sealed trait Change[T]
//  final case class Folder [T <: Txn[T]](peer: stm.Folder.Update[T])  extends Change[T]
//  final case class Offset [T <: Txn[T]](peer: model.Change[Long   ]) extends Change[T]
//  final case class Playing[T <: Txn[T]](peer: model.Change[Boolean]) extends Change[T]
//
//  def readIdentifiedObj[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): Obj[T] =
//    Impl.readIdentifiedObj(in, access)
//}
//
///** An `Ensemble` is sort of a persistent transport model.
//  * It maintains a list of transported objects through the
//  * `folder` member. The `playing` expression determines
//  * when the transport is playing or not. Upon a transition
//  * from stopped to playing, the `offset` member determines
//  * the "seek" position.
//  */
//@deprecated("Should only use Folder now with Control/Action", since = "3.35.3")
//trait Ensemble[T <: Txn[T]] extends Obj[T] with Publisher[T, Ensemble.Update[T]] {
//  def folder (implicit tx: T): Folder /* Elem.Obj */ [T]
//  def offset (implicit tx: T): LongObj[T]
//  def playing(implicit tx: T): BooleanObj[T]
//}
