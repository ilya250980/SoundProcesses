///*
// *  ActionRaw.scala
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
//import de.sciss.lucre.stm.{Obj, Sys, TxnLike}
//import de.sciss.lucre.{stm, event => evt}
//import de.sciss.serial.{DataInput, TFormat}
//import de.sciss.synth.proc.impl.{ActionRawImpl => Impl}
//
//import scala.collection.immutable.{IndexedSeq => Vec}
//import scala.concurrent.Future
//
///** The old `Action` object backed up by raw Scala code.
//  */
//@deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
//object ActionRaw extends Obj.Type {
//  final val typeId = 19
//
//  final val attrSource = "action-source"
//
//  def compile[T <: Txn[T]](source: Code.ActionRaw)
//                          (implicit tx: T, cursor: Cursor[T],
//                           compiler: Code.Compiler): Future[stm.Source[T, ActionRaw[T]]] =
//    Impl.compile(source)
//
//  def empty[T <: Txn[T]](implicit tx: T): ActionRaw[T] = Impl.empty[T]
//
//  def mkName[T <: Txn[T]]()(implicit tx: T): String = Impl.mkName[T]()
//
//  def newConst[T <: Txn[T]](name: String, jar: Array[Byte])(implicit tx: T): ActionRaw[T] =
//    Impl.newConst(name, jar)
//
//  def predef[T <: Txn[T]](id: String)(implicit tx: T): ActionRaw[T] = Impl.predef(id)
//
//  def registerPredef(id: String, body: Action.Body)(implicit tx: TxnLike): Unit = Impl.registerPredef(id, body)
//
//  object Var {
//    def apply[T <: Txn[T]](init: ActionRaw[T])(implicit tx: T): Var[T] = Impl.newVar(init)
//
//    def unapply[T <: Txn[T]](a: ActionRaw[T]): Option[Var[T]] =
//      a match {
//        case x: Var[T] => Some(x)
//        case _ => None
//      }
//
//    implicit def format[T <: Txn[T]]: TFormat[T, Var[T]] = Impl.varFormat[T]
//  }
//  trait Var[T <: Txn[T]] extends ActionRaw[T] with stm.Var[T, ActionRaw[T]]
//
//  implicit def format[T <: Txn[T]]: TFormat[T, ActionRaw[T]] = Impl.format[T]
//
//  def read[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): ActionRaw[T] = format[T].read(in, access)
//
//  // ---- body ----
//
//  /** Possible type for `universe.value`. Overcomes erasure. */
//  final case class DoubleVector(xs: Vec[Double])
//  /** Possible type for `universe.value`. Overcomes erasure. */
//  final case class FloatVector (xs: Vec[Float ])
//
//  def readIdentifiedObj[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): Obj[T] =
//    Impl.readIdentifiedObj(in, access)
//
//  type Body                           = Action.Body
//  type Universe[T <: Txn[T]]          = Action.Universe[T]
//  val  Universe: Action.Universe.type = Action.Universe
//}
//@deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
//trait ActionRaw[T <: Txn[T]] extends Obj[T] with evt.Publisher[T, Unit] {
//  def execute(universe: Action.Universe[T])(implicit tx: T): Unit
//}