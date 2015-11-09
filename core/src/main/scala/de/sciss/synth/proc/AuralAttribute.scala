/*
 *  AuralAttribute.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Disposable, Sys}
import de.sciss.lucre.synth.{Sys => SSys, Txn, AuralNode, NodeRef, AudioBus}
import de.sciss.synth.ControlSet
import impl.{AuralAttributeImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.{implicitConversions, higherKinds}

object AuralAttribute {
  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: SSys[S]](value: Repr[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: SSys[S]](value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = Impl(value)

  // ---- Target ----

  object Target {
    def apply(nodeRef: NodeRef.Full, key: String, targetBus: AudioBus)(implicit tx: Txn): Target = {
      val res = new impl.AuralAttributeTargetImpl(nodeRef, key, targetBus)
      nodeRef.addUser(res)
      res
    }
  }
  trait Target {
    def put   (instance: Instance, value: Value)(implicit tx: Txn): Unit

    def add   (instance: Instance)(implicit tx: Txn): Unit
    def remove(instance: Instance)(implicit tx: Txn): Unit
  }

  // ---- Value ----

  object Value {
    implicit def fromFloat (value : Float     ): ScalarValue  = new ScalarValue (value )
    implicit def fromFloats(values: Vec[Float]): ScalarVector = new ScalarVector(values)
  }
  sealed trait Value { def isScalar: Boolean }
  /** Value for which a no synth is needed, but only a scalar value
    * that needs to be set on the target node.
    */
  sealed trait Scalar extends Value {
    def toControl(key: String, numChannels: Int): ControlSet
    def values: Vec[Float]
    final def isScalar = true
  }

  final case class ScalarValue(value: Float) extends Scalar {
    def toControl(key: String, numChannels: Int): ControlSet =
      if (numChannels == 1) ControlSet.Value (key, value)
      else                  ControlSet.Vector(key, Vector.fill(numChannels)(value))

    def values: Vec[Float] = Vector(value)
  }
  final case class ScalarVector(values: Vec[Float]) extends Scalar {
    def toControl(key: String, numChannels: Int): ControlSet = {
      val sz = values.size
      val xs = if (numChannels == sz) values else Vector.tabulate(numChannels)(i => values(i % sz))
      ControlSet.Vector(key, xs)
    }
  }

  /** Value for which a `Synth` is required that writes its signal to a bus,
    * and the bus is then somehow mapped to the target node's control.
    */
  final case class Stream(source: NodeRef, bus: AudioBus) extends Value {
    def isScalar = false
  }

  // trait Instance[S <: Sys[S]] extends Disposable[S#Tx]
  trait Instance extends Disposable[Txn]
}
trait AuralAttribute[S <: Sys[S]] extends Disposable[S#Tx] {
  import AuralAttribute.Target

  def preferredNumChannels(implicit tx: S#Tx): Int

  // def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit

  def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit // Instance[S]

  // def stop   ()(implicit tx: S#Tx): Unit
}