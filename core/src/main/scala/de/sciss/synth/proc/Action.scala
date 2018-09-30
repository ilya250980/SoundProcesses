/*
 *  Action.scala
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

import de.sciss.lucre.stm.{Folder, Obj, Sys, TxnLike}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.serial.{DataInput, Serializer}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{ActionImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.Future

object Action extends Obj.Type {
  final val typeId = 19

  final val attrSource = "action-source"

  def compile[S <: Sys[S]](source: Code.Action)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S],
                           compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] =
    Impl.compile(source)

  def empty[S <: Sys[S]](implicit tx: S#Tx): Action[S] = Impl.empty[S]

  def mkName[S <: Sys[S]]()(implicit tx: S#Tx): String = Impl.mkName[S]()

  def newConst[S <: Sys[S]](name: String, jar: Array[Byte])(implicit tx: S#Tx): Action[S] =
    Impl.newConst(name, jar)

  def predef[S <: Sys[S]](id: String)(implicit tx: S#Tx): Action[S] = Impl.predef(id)

  def registerPredef(id: String, body: Body)(implicit tx: TxnLike): Unit = Impl.registerPredef(id, body)

  object Var {
    def apply[S <: Sys[S]](init: Action[S])(implicit tx: S#Tx): Var[S] = Impl.newVar(init)

    def unapply[S <: Sys[S]](a: Action[S]): Option[Var[S]] =
      a match {
        case x: Var[S] => Some(x)
        case _ => None
      }

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Var[S]] = Impl.varSerializer[S]
  }
  trait Var[S <: Sys[S]] extends Action[S] with stm.Var[S#Tx, Action[S]]

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action[S]] = Impl.serializer[S]

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action[S] = serializer[S].read(in, access)

  // ---- body ----

  trait Body {
    def apply[S <: Sys[S]](universe: Universe[S])(implicit tx: S#Tx): Unit
  }

  /** Possible type for `universe.value`. Overcomes erasure. */
  final case class DoubleVector(xs: Vec[Double])
  /** Possible type for `universe.value`. Overcomes erasure. */
  final case class FloatVector (xs: Vec[Float ])

  object Universe {
    def apply[S <: SSys[S]](self: Action[S], invoker: Option[Obj[S]] = None, value: Any = ())
                           (implicit peer: proc.Universe[S]): Universe[S] =
      new Impl.UniverseImpl(self, invoker, value)
  }
  trait Universe[S <: Sys[S]] extends proc.Universe[S] {
    /** The action object itself, most prominently giving access to
      * linked objects via its attributes.
      */
    def self: Action[S]

    /** A result object from the invoker. To permit different kind of invocations,
      * this value is untyped. Conventionally, `Action.DoubleVector` and `Action.FloatVector`
      * are used for collections of numbers.
      */
    def value: Any

    /** Parent component from which the action is invoked. For example
      * if used from within a synth-graph, this will be some `Proc.Obj`.
      * `None` if the action is directly invoked without dedicated parent.
      */
    def invoker: Option[Obj[S]]

    def root(implicit tx: S#Tx): Folder[S] = workspace.root

    def log(what: => String)(implicit tx: S#Tx): Unit
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Obj[S] =
    Impl.readIdentifiedObj(in, access)
}
trait Action[S <: Sys[S]] extends Obj[S] with evt.Publisher[S, Unit] {
  def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit
}