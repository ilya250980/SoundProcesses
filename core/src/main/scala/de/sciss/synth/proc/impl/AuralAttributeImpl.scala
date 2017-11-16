/*
 *  AuralAttributeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, DoubleVector, Expr, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.Curve
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, Scalar, ScalarOptionView, Target}
import de.sciss.synth.proc.AuralView.{Playing, Prepared, State, Stopped}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  import TxnLike.peer

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](key: String, value: Obj[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val tid = value.tpe.typeID
    map.get(tid) match {
      case Some(factory) =>
        factory(key, value.asInstanceOf[factory.Repr[S]], observer)
      case None =>
        Console.err.println(s"Warning: AuralAttribute - no factory for ${value.tpe}")
        new DummyAttribute[S](key, tx.newHandle(value))
    }
  }

  private[this] var map = Map[Int, Factory](
    IntObj      .typeID -> IntAttribute,
    DoubleObj   .typeID -> DoubleAttribute,
    BooleanObj  .typeID -> BooleanAttribute,
    FadeSpec    .typeID -> FadeSpecAttribute,
    DoubleVector.typeID -> DoubleVectorAttribute,
    Grapheme    .typeID -> AuralGraphemeAttribute,
    Output      .typeID -> AuralOutputAttribute,
    Folder      .typeID -> AuralFolderAttribute,
    Timeline    .typeID -> AuralTimelineAttribute,
    EnvSegment  .typeID -> AuralEnvSegmentAttribute
  )

  private final class Playing[S <: Sys[S]](val timeRef: TimeRef, val wallClock: Long, val target: Target[S]) {
    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }
  }

  trait ExprImpl[S <: Sys[S], A]
    extends AuralAttributeImpl[S] { attr =>

    // ---- abstract ----

    protected val context: AuralContext[S]

    /* override */ def obj: stm.Source[S#Tx, Expr[S, A]]

    protected def mkValue(timeRef: TimeRef, in: A)(implicit tx: S#Tx): AuralAttribute.Value

    // ---- impl ----

    import context.{scheduler => sched}

    private[this] var obs: Disposable[S#Tx] = _
    private[this] val playRef = Ref(Option.empty[Playing[S]])

    final def targetOption(implicit tx: S#Tx): Option[Target[S]] = playRef().map(_.target)

    final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = state = Prepared

    final def play(timeRef: TimeRef.Option, target: Target[S])(implicit tx: S#Tx): Unit /* Instance */ = {
      val playing = new Playing(timeRef.force, sched.time, target)
      require(playRef.swap(Some(playing))(tx.peer).isEmpty)
      state = Playing
      updateTarget(playing.timeRef, playing.target, obj().value)
    }

    final def stop()(implicit tx: S#Tx): Unit = {
      stopNoFire()
      state = Stopped
    }

    private def updateTarget(timeRef: TimeRef, target: Target[S], value: A)(implicit tx: S#Tx): Unit = {
      val ctlVal = mkValue(timeRef, value)
      target.put(this, ctlVal)
    }

    final protected def valueChanged(value: A)(implicit tx: S#Tx): Unit = {
      playRef().foreach { p =>
        val trNew = p.shiftTo(sched.time)
        updateTarget(trNew, p.target, value)
      }
    }

    def init(expr: Expr[S, A])(implicit tx: S#Tx): this.type = {
      obs = expr.changed.react { implicit tx => change =>
        valueChanged(change.now)
      }
      this
    }

    private def stopNoFire()(implicit tx: S#Tx): Unit =
      playRef.swap(None).foreach(p => p.target.remove(this))

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      stopNoFire()
    }
  }

  private trait SingleChannelImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    final def preferredNumChannels(implicit tx: S#Tx): Int = 1
  }

  private final class NumericExprObserver[S <: Sys[S], A](expr: NumericExprImpl[S, A])
    extends ObservableImpl[S, Option[Scalar]] with ScalarOptionView[S] {

    def apply()(implicit tx: S#Tx): Option[Scalar] = Some(expr.mkValue(expr.obj().value))
  }

  private trait NumericExprImpl[S <: Sys[S], A] extends ExprImpl[S, A] with AuralAttribute.StartLevelSource[S] {
    def mkValue(in: A)(implicit tx: S#Tx): AuralAttribute.Scalar

    final def mkValue(timeRef: TimeRef, value: A)(implicit tx: S#Tx): AuralAttribute.Value = mkValue(value)

    def startLevel(implicit tx: S#Tx): ScalarOptionView[S] = new NumericExprObserver(this)
  }

  // ------------------- IntObj -------------------

  private[this] object IntAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = IntObj[S]

    def typeID: Int = IntObj.typeID

    def apply[S <: Sys[S]](key: String, value: IntObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new IntAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class IntAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, IntObj[S]])
                                               (implicit val context: AuralContext[S])
    extends SingleChannelImpl[S, Int] with NumericExprImpl[S, Int] {

    def typeID: Int = IntObj.typeID

    def mkValue(value: Int)(implicit tx: S#Tx): AuralAttribute.Scalar = value.toFloat

    override def toString = s"IntAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- DoubleObj ------------------- 

  private[this] object DoubleAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = DoubleObj[S]

    def typeID: Int = DoubleObj.typeID

    def apply[S <: Sys[S]](key: String, value: DoubleObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class DoubleAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, DoubleObj[S]])
                                                  (implicit val context: AuralContext[S])
    extends SingleChannelImpl[S, Double] with NumericExprImpl[S, Double] {

    def typeID: Int = DoubleObj.typeID

    def mkValue(value: Double)(implicit tx: S#Tx): AuralAttribute.Scalar = value.toFloat

    override def toString = s"DoubleAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- BooleanObj ------------------- 

  private[this] object BooleanAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = BooleanObj[S]

    def typeID: Int = BooleanObj.typeID

    def apply[S <: Sys[S]](key: String, value: BooleanObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new BooleanAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class BooleanAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, BooleanObj[S]])
                                                   (implicit val context: AuralContext[S])
    extends SingleChannelImpl[S, Boolean] with NumericExprImpl[S, Boolean] {

    def typeID: Int = BooleanObj.typeID

    def mkValue(value: Boolean)(implicit tx: S#Tx): AuralAttribute.Scalar = if (value) 1f else 0f

    override def toString = s"BooleanAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- FadeSpec.Obj ------------------- 

  private[this] object FadeSpecAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = FadeSpec.Obj[S]

    def typeID: Int = FadeSpec.Obj.typeID

    def apply[S <: Sys[S]](key: String, value: FadeSpec.Obj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new FadeSpecAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class FadeSpecAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, FadeSpec.Obj[S]])
                                                    (implicit val context: AuralContext[S])
    extends ExprImpl[S, FadeSpec] {

    def typeID: Int = FadeSpec.Obj.typeID

    def preferredNumChannels(implicit tx: S#Tx): Int = 4

    def mkValue(timeRef: TimeRef, spec: FadeSpec)(implicit tx: S#Tx): AuralAttribute.Scalar = {
      val v = Vector[Float](
        (spec.numFrames / TimeRef.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
          case Curve.parametric(c)  => c
          case _                    => 0f
        }, spec.floor
      )
      v
    }

    override def toString = s"FadeSpecAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- DoubleVector ------------------- 

  private[this] object DoubleVectorAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = DoubleVector[S]

    def typeID: Int = DoubleVector.typeID

    def apply[S <: Sys[S]](key: String, value: DoubleVector[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleVectorAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class DoubleVectorAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, DoubleVector[S]])
                                                        (implicit val context: AuralContext[S])
    extends ExprImpl[S, Vec[Double]] with NumericExprImpl[S, Vec[Double]] {

    def typeID: Int = DoubleVector.typeID

    def preferredNumChannels(implicit tx: S#Tx): Int = obj().value.size

    def mkValue(vec: Vec[Double])(implicit tx: S#Tx): AuralAttribute.Scalar = vec.map(_.toFloat)

    override def toString = s"DoubleVectorAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- generic (dummy) -------------------
  private final class DummyAttribute[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Obj[S]])
    extends AuralAttribute[S] {

    def typeID: Int = throw new UnsupportedOperationException("DummyAttribute.typeID")

    def preferredNumChannels(implicit tx: S#Tx): Int = 0

    def targetOption(implicit tx: S#Tx): Option[Target[S]] = None

    def state(implicit tx: S#Tx): State = Stopped

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = ()

    def play(timeRef: TimeRef.Option, target: Target[S])(implicit tx: S#Tx): Unit = ()

    def stop   ()(implicit tx: S#Tx): Unit = ()
    def dispose()(implicit tx: S#Tx): Unit = ()

    def react(fun: (S#Tx) => (State) => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = DummyObservableImpl

    override def toString = s"DummyAttribute($key)@${hashCode.toHexString}"
  }
}
trait AuralAttributeImpl[S <: Sys[S]] extends AuralAttribute[S] with ObservableImpl[S, AuralView.State] {
  import TxnLike.peer

  private[this] final val stateRef = Ref[State](Stopped)

  final def state(implicit tx: S#Tx): State = stateRef()
  final protected def state_=(value: State)(implicit tx: S#Tx): Unit = {
    val prev = stateRef.swap(value)
    if (value != prev) fire(value)
  }
}