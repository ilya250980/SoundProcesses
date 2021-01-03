/*
 *  AuralAttribute.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.synth.{AudioBus, NodeRef}
import de.sciss.lucre.{IExpr, Obj, Observable, Txn, synth}
import de.sciss.synth.ControlSet
import de.sciss.proc.impl.{AuralAttributeImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object AuralAttribute {
  def apply[T <: synth.Txn[T]](key: String, value: Obj[T], observer: Observer[T])
                         (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
    Impl(key, value, observer)

  /** Attempts to create an aural attribute from an in-memory expression */
  def expr[T <: synth.Txn[T], A](key: String, value: IExpr[T, A], observer: Observer[T])
                           (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
    Impl.expr(key, value, observer)

  // ---- Observer ----

  trait Observer[T <: Txn[T]] {
    def attrNumChannelsChanged(attr: AuralAttribute[T])(implicit tx: T): Unit
  }

  // ---- Factory ----

  trait Factory {
    def tpe: Obj.Type

    type Repr[~ <: Txn[~]] <: Obj[~]

    def apply[T <: synth.Txn[T]](key: String, value: Repr[T], observer: Observer[T])
                           (implicit tx: T, context: AuralContext[T]): AuralAttribute[T]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  // ---- Target ----

  object Target {
    def apply[T <: synth.Txn[T]](nodeRef: NodeRef.Full[T], key: String, targetBus: AudioBus)
                           (implicit tx: T): Target[T] = {
      val res = new impl.AuralAttributeTargetImpl[T](nodeRef, key, targetBus)
      // nodeRef.addUser(res)
      res
    }
  }

  /** An `AuralAttribute.Target` describes the mechanism by which
    * the attribute inputs can contribute their values. It is internally
    * connected to the process's node. One or multiple inputs then
    * supply their values by calling `put` and detach themselves by
    * calling `remove`. The target automatically manages summing multiple
    * inputs. While the `Value` parameter for `put` corresponds to
    * a particular attribute input, its `valueOption` gives the
    * overall signal output as sent to the node. For instance,
    * a stream input will have a bus to which is ''writes'', whereas
    * the target itself may provide a bus from which this node ''reads''.
    *
    * It is possible for an attribute to call `put` repeatedly, the
    * target will then automatically dispose the previous value it
    * had associated with that attribute.
    */
  trait Target[T <: Txn[T]] extends Observable[T, Value] {
    def key: String

    def valueOption(implicit tx: T): Option[Value]

    def put   (attr: AuralAttribute[T], value: Value)(implicit tx: T): Unit
    def remove(attr: AuralAttribute[T]              )(implicit tx: T): Unit
  }

  // ---- Value ----

  object Value {
    implicit def fromFloat (value : Float     ): ScalarValue  = ScalarValue (value )
    implicit def fromFloats(values: Vec[Float]): ScalarVector = ScalarVector(values)
  }
  sealed trait Value { def isScalar: Boolean }
  /** Value for which a no synth is needed, but only a scalar value
    * that needs to be set on the target node.
    */
  sealed trait Scalar extends Value {
    def toControl(key: String, numChannels: Int): ControlSet
    def values: Vec[Float]
    final def isScalar = true
    def numChannels: Int
  }

  final case class ScalarValue(value: Float) extends Scalar {
    def toControl(key: String, numChannels: Int): ControlSet =
      if (numChannels == 1) ControlSet.Value (key, value)
      else                  ControlSet.Vector(key, Vector.fill(numChannels)(value))

    def values: Vec[Float] = Vector(value)

    def numChannels = 1
  }
  final case class ScalarVector(values: Vec[Float]) extends Scalar {
    def toControl(key: String, numChannels: Int): ControlSet = {
      val sz = values.size
      val xs = if (numChannels == sz) values else Vector.tabulate(numChannels)(i => values(i % sz))
      ControlSet.Vector(key, xs)
    }

    def numChannels: Int = values.size
  }

  /** Value for which a `Synth` is required that writes its signal to a bus,
    * and the bus is then somehow mapped to the target node's control.
    */
  final case class Stream(source: NodeRef, bus: AudioBus) extends Value {
    def isScalar = false
  }

  // ---- StartLevel / EndLevel ----

  def addStartLevelViewFactory(f: StartLevelViewFactory): Unit = Impl.addStartLevelViewFactory(f)

  def startLevelView[T <: synth.Txn[T]](obj: Obj[T])(implicit tx: T): ControlValuesView[T] = Impl.startLevelView[T](obj)

  trait GraphemeAware[T <: Txn[T]] {
    def setGrapheme(pos: Long, g: Grapheme[T])(implicit tx: T): Unit
  }
}
trait AuralAttribute[T <: Txn[T]] extends /*Obj*/ AuralViewBase[T, AuralAttribute.Target[T]] {
  def key: String

  /** Or `-1` if the number of channels cannot be determined. */
  def preferredNumChannels(implicit tx: T): Int

  def targetOption(implicit tx: T): Option[AuralAttribute.Target[T]]
}