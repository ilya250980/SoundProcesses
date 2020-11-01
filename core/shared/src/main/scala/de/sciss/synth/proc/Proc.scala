/*
 *  Proc.scala
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

import de.sciss.lucre.{Obj, Publisher, Txn}
import de.sciss.model
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.impl.{OutputImpl, ProcImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}

object Proc extends Obj.Type {
  final val typeId = 0x10005

  // ---- implementation forwards ----

  override def init(): Unit = {
    super.init()
    Output.init()
//    AuralObjFactory.init()
  }

  def apply[T <: Txn[T]]()(implicit tx: T): Proc[T] = Impl[T]()

  def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Proc[T] = Impl.read(in)

  implicit def format[T <: Txn[T]]: TFormat[T, Proc[T]] = Impl.format[T]

  // ---- event types ----

  /** An update is a sequence of changes */
  final case class Update[T <: Txn[T]](proc: Proc[T], changes: Vec[Change[T]])

  /** A change is either a state change, or a scan or a grapheme change */
  sealed trait Change[T <: Txn[T]]

  final case class GraphChange[T <: Txn[T]](change: model.Change[SynthGraph]) extends Change[T]

  /** An associative change is either adding or removing an association */
  sealed trait OutputsChange[T <: Txn[T]] extends Change[T] {
    def output: Output[T]
  }

  final case class OutputAdded  [T <: Txn[T]](output: Output[T]) extends OutputsChange[T]
  final case class OutputRemoved[T <: Txn[T]](output: Output[T]) extends OutputsChange[T]

  /** Source code of the graph function. */
  final val attrSource = "graph-source"

  final val mainIn  = "in"
  final val mainOut = "out"

  /** Audio input file (tape) grapheme. */
  final val graphAudio = "sig"

  /** NOT USED ANY LONGER. Hint key for copying scan connections during `copy`. Value should be a
    * predicate function `(Proc[T]) => Boolean`. If absent, all connections
    * are copied.
    */
  final val hintFilterLinks = "links"

  override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
    Impl.readIdentifiedObj(in)

  // ---- Outputs ----

  import de.sciss.lucre.{Obj, Txn}
  import de.sciss.serial.{DataInput, TFormat}

  object Output extends Obj.Type {
    final val typeId = 0x10009

    def read[T <: Txn[T]](in: DataInput)(implicit tx: T): Output[T] = OutputImpl.read(in)

    implicit def format[T <: Txn[T]]: TFormat[T, Output[T]] = OutputImpl.format

    override def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Obj[T] =
      OutputImpl.readIdentifiedObj(in)
  }
  trait Output[T <: Txn[T]] extends Obj[T] {
    def proc: Proc[T]
    def key : String  // or `StringObj`?
  }

  trait Outputs[T <: Txn[T]] {
    def get(key: String)(implicit tx: T): Option[Output[T]]

    def keys(implicit tx: T): Set[String]

    def iterator(implicit tx: T): Iterator[Output[T]]

    /** Adds a new scan by the given key. If a span by that name already exists, the old scan is returned. */
    def add   (key: String)(implicit tx: T): Output[T]

    def remove(key: String)(implicit tx: T): Boolean
  }

  // AuralObj.Factory
}

/** The `Proc` trait is the basic entity representing a sound process. */
trait Proc[T <: Txn[T]] extends Obj[T] with Publisher[T, Proc.Update[T]] {
  /** The variable synth graph function of the process. */
  def graph: SynthGraphObj.Var[T]

  /** The real-time outputs of the process. */
  def outputs: Proc.Outputs[T]
}