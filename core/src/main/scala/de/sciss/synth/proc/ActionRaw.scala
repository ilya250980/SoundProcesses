/*
 *  ActionRaw.scala
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

import de.sciss.lucre.stm.{Obj, Sys, TxnLike}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc.impl.{ActionRawImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

/** The old `Action` object backed up by raw Scala code.
  */
@deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
object ActionRaw extends Obj.Type {
  final val typeId = 19

  final val attrSource = "action-source"

  def compile[S <: Sys[S]](source: Code.ActionRaw)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S],
                           compiler: Code.Compiler): Future[stm.Source[S#Tx, ActionRaw[S]]] =
    Impl.compile(source)

  def empty[S <: Sys[S]](implicit tx: S#Tx): ActionRaw[S] = Impl.empty[S]

  def mkName[S <: Sys[S]]()(implicit tx: S#Tx): String = Impl.mkName[S]()

  def newConst[S <: Sys[S]](name: String, jar: Array[Byte])(implicit tx: S#Tx): ActionRaw[S] =
    Impl.newConst(name, jar)

  def predef[S <: Sys[S]](id: String)(implicit tx: S#Tx): ActionRaw[S] = Impl.predef(id)

  def registerPredef(id: String, body: Action.Body)(implicit tx: TxnLike): Unit = Impl.registerPredef(id, body)

  object Var {
    def apply[S <: Sys[S]](init: ActionRaw[S])(implicit tx: S#Tx): Var[S] = Impl.newVar(init)

    def unapply[S <: Sys[S]](a: ActionRaw[S]): Option[Var[S]] =
      a match {
        case x: Var[S] => Some(x)
        case _ => None
      }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Var[S]] = Impl.varSerializer[S]
  }
  trait Var[S <: Sys[S]] extends ActionRaw[S] with stm.Var[S#Tx, ActionRaw[S]]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, ActionRaw[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): ActionRaw[S] = serializer[S].read(in, access)

  // ---- body ----

  /** Possible type for `universe.value`. Overcomes erasure. */
  final case class DoubleVector(xs: Vec[Double])
  /** Possible type for `universe.value`. Overcomes erasure. */
  final case class FloatVector (xs: Vec[Float ])

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)

  type Body                           = Action.Body
  type Universe[S <: Sys[S]]          = Action.Universe[S]
  val  Universe: Action.Universe.type = Action.Universe
}
@deprecated("Action should be used instead of ActionRaw", since = "3.31.0")
trait ActionRaw[S <: Sys[S]] extends Obj[S] with evt.Publisher[S, Unit] {
  def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit
}