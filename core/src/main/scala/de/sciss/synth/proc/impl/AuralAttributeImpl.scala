/*
 *  AuralAttributeImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.event.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, DoubleVector, Expr, IntObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Folder, NoSys, Obj, TxnLike}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.Curve
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, Scalar, Target}
import de.sciss.synth.proc.Runner.{Prepared, Running, State, Stopped}
import de.sciss.synth.proc.{AuralAttribute, AuralContext, ControlValuesView, EnvSegment, FadeSpec, Grapheme, Output, Runner, StartLevelViewFactory, TimeRef, Timeline}
import de.sciss.synth.ugen.ControlValues

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  import TxnLike.peer

  // ---- Factory ----

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](key: String, value: Obj[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val tid = value.tpe.typeId
    map.get(tid) match {
      case Some(factory) =>
        factory(key, value.asInstanceOf[factory.Repr[S]], observer)
      case None =>
        Console.err.println(s"Warning: AuralAttribute - no factory for ${value.tpe}")
        new DummyAttribute[S](key, tx.newHandle(value))
    }
  }

  private[this] var map = Map[Int, Factory](
    IntObj      .typeId -> IntAttribute,
    DoubleObj   .typeId -> DoubleAttribute,
    BooleanObj  .typeId -> BooleanAttribute,
    FadeSpec    .typeId -> FadeSpecAttribute,
    DoubleVector.typeId -> DoubleVectorAttribute,
    Grapheme    .typeId -> AuralGraphemeAttribute,
    Output      .typeId -> AuralOutputAttribute,
    Folder      .typeId -> AuralFolderAttribute,
    Timeline    .typeId -> AuralTimelineAttribute,
    EnvSegment  .typeId -> AuralEnvSegmentAttribute
  )

  // ---- StartLevel ----

  private[this] var startLevelMap = Map[Int, StartLevelViewFactory](
    IntObj      .typeId -> IntAttribute,
    DoubleObj   .typeId -> DoubleAttribute,
    BooleanObj  .typeId -> BooleanAttribute,
    DoubleVector.typeId -> DoubleVectorAttribute,
//    Grapheme    .typeId -> AuralGraphemeAttribute,
    EnvSegment  .typeId -> AuralEnvSegmentAttribute
  )

  def addStartLevelViewFactory(f: StartLevelViewFactory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (startLevelMap.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    startLevelMap += tid -> f
  }

  def startLevelView[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ControlValuesView[S] = {
    val tid = obj.tpe.typeId
    startLevelMap.get(tid) match {
      case Some(factory) =>
        factory.mkStartLevelView(obj.asInstanceOf[factory.Repr[S]])
      case None =>
        DummyScalarOptionView.asInstanceOf[ControlValuesView[S]]
    }
  }

  private object DummyScalarOptionView extends ControlValuesView[NoSys] with DummyObservableImpl[NoSys] {
    def apply()(implicit tx: NoSys#Tx): Option[ControlValues] = None
  }

  // ---- ----

  private final class Playing[S <: Sys[S]](val timeRef: TimeRef, val wallClock: Long,
                                           val target: Target[S], val value: AuralAttribute.Value) {
    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    def updateValue(newValue: AuralAttribute.Value): Playing[S] =
      new Playing(timeRef, wallClock, target, newValue)
  }

  trait ExprImpl[S <: Sys[S], A]
    extends AuralAttributeImpl[S] { attr =>

    // ---- abstract ----

    protected val context: AuralContext[S]

    def objH: stm.Source[S#Tx, Expr[S, A]]

    /** Creates a value representation of this input. If the value is a `Stream`,
      * that stream's node reference will be disposed when the input stops (is replaced by other input).
      */
    protected def mkValue(timeRef: TimeRef, in: A)(implicit tx: S#Tx): AuralAttribute.Value

    // ---- impl ----

//    import context.{scheduler => sched}
    import context.universe.{scheduler => sched}

    private[this] var obs: Disposable[S#Tx] = _
    private[this] val playRef = Ref(Option.empty[Playing[S]])

    final def targetOption(implicit tx: S#Tx): Option[Target[S]] = playRef().map(_.target)

    final def prepare(timeRef: TimeRef.Option, attr: Map[String, Any])(implicit tx: S#Tx): Unit = state = Prepared

    final def run(timeRef: TimeRef.Option, target: Target[S])(implicit tx: S#Tx): Unit /* Instance */ = {
      state = Running
      val timeF   = timeRef.force
      val value   = updateTarget(timeF, target, objH().value)
      val playing = new Playing(timeF, sched.time, target, value)
      require(playRef.swap(Some(playing))(tx.peer).isEmpty)
    }

    final def stop()(implicit tx: S#Tx): Unit = {
      stopNoFire()
      state = Stopped
    }

    private def updateTarget(timeRef: TimeRef, target: Target[S], value: A)
                            (implicit tx: S#Tx): AuralAttribute.Value = {
      val ctlVal = mkValue(timeRef, value)
      target.put(this, ctlVal)
      ctlVal
    }

    final protected def valueChanged(value: A)(implicit tx: S#Tx): Unit = {
      playRef().foreach { p =>
        val trNew     = p.shiftTo(sched.time)
        val newValue  = updateTarget(trNew, p.target, value)
        if (!(p.value.isScalar && newValue.isScalar)) {
          freeValue(p)
          val newP = p.updateValue(newValue)
          playRef() = Some(newP)
        }
      }
    }

    def init(expr: Expr[S, A])(implicit tx: S#Tx): this.type = {
      obs = expr.changed.react { implicit tx => change =>
        valueChanged(change.now)
      }
      this
    }

    @inline
    private def freeValue(p: Playing[S])(implicit tx: S#Tx): Unit = p.value match {
      case AuralAttribute.Stream(nodeRef, _) => nodeRef.node.dispose()
      case _ =>
    }

    private def stopNoFire()(implicit tx: S#Tx): Unit =
      playRef.swap(None).foreach { p =>
        p.target.remove(this)
        freeValue(p)
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      stopNoFire()
    }
  }

  private trait SingleChannelImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    final def preferredNumChannels(implicit tx: S#Tx): Int = 1
  }

  private abstract class NumericExprStartLevel[S <: Sys[S], A](obj: stm.Source[S#Tx, Expr[S, A]])
    extends ControlValuesView[S] {

    def mkValue(in: A): ControlValues

    def apply()(implicit tx: S#Tx): Option[ControlValues] = Some(mkValue(obj().value))

    def react(fun: S#Tx => Option[ControlValues] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      obj().changed.react { implicit tx => ch =>
        val lvlCh = ch.map(mkValue)
        if (lvlCh.isSignificant) fun(tx)(Some(lvlCh.now))
      }
  }

  private trait NumericExprImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    def mkValue(in: A): AuralAttribute.Scalar

    final def mkValue(timeRef: TimeRef, value: A)(implicit tx: S#Tx): AuralAttribute.Value = mkValue(value)
  }

  // ------------------- IntObj -------------------

  private[this] object IntAttribute extends Factory with StartLevelViewFactory {
    type Repr[S <: stm.Sys[S]] = IntObj[S]

    def tpe: Obj.Type = IntObj

    def apply[S <: Sys[S]](key: String, value: IntObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new IntAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[S <: Sys[S]](value: Repr[S])(implicit tx: S#Tx): ControlValuesView[S] =
      new IntStartLevel(tx.newHandle(value))
  }

  private final class IntStartLevel[S <: Sys[S]](obj: stm.Source[S#Tx, IntObj[S]])
    extends NumericExprStartLevel(obj) {

    def mkValue(in: Int): ControlValues = in.toFloat
  }
  
  private final class IntAttribute[S <: Sys[S]](val key: String, val objH: stm.Source[S#Tx, IntObj[S]])
                                               (implicit val context: AuralContext[S])
    extends SingleChannelImpl[S, Int] with NumericExprImpl[S, Int] {

    def tpe: Obj.Type = IntObj

    def mkValue(in: Int): Scalar = in.toFloat

    override def toString = s"IntAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- DoubleObj ------------------- 

  private[this] object DoubleAttribute extends Factory with StartLevelViewFactory  {
    type Repr[S <: stm.Sys[S]] = DoubleObj[S]

    def tpe: Obj.Type = DoubleObj

    def apply[S <: Sys[S]](key: String, value: DoubleObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[S <: Sys[S]](value: Repr[S])(implicit tx: S#Tx): ControlValuesView[S] =
      new DoubleStartLevel(tx.newHandle(value))
  }

  private final class DoubleStartLevel[S <: Sys[S]](obj: stm.Source[S#Tx, DoubleObj[S]])
    extends NumericExprStartLevel(obj) {

    def mkValue(in: Double): ControlValues = in
  }
  
  private final class DoubleAttribute[S <: Sys[S]](val key: String, val objH: stm.Source[S#Tx, DoubleObj[S]])
                                                  (implicit val context: AuralContext[S])
    extends SingleChannelImpl[S, Double] with NumericExprImpl[S, Double] {

    def tpe: Obj.Type = DoubleObj

    def mkValue(value: Double): Scalar = value.toFloat

    override def toString = s"DoubleAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- BooleanObj ------------------- 

  private[this] object BooleanAttribute extends Factory with StartLevelViewFactory {
    type Repr[S <: stm.Sys[S]] = BooleanObj[S]

    def tpe: Obj.Type = BooleanObj

    def apply[S <: Sys[S]](key: String, value: BooleanObj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new BooleanAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[S <: Sys[S]](value: Repr[S])(implicit tx: S#Tx): ControlValuesView[S] =
      new BooleanStartLevel(tx.newHandle(value))
  }

  private final class BooleanStartLevel[S <: Sys[S]](obj: stm.Source[S#Tx, BooleanObj[S]])
    extends NumericExprStartLevel(obj) {

    def mkValue(in: Boolean): ControlValues = if (in) 1f else 0f
  }
  
  private final class BooleanAttribute[S <: Sys[S]](val key: String, val objH: stm.Source[S#Tx, BooleanObj[S]])
                                                   (implicit val context: AuralContext[S])
    extends SingleChannelImpl[S, Boolean] with NumericExprImpl[S, Boolean] {

    def tpe: Obj.Type = BooleanObj

    def mkValue(in: Boolean): Scalar = if (in) 1f else 0f

    override def toString = s"BooleanAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- FadeSpec.Obj ------------------- 

  private[this] object FadeSpecAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = FadeSpec.Obj[S]

    def tpe: Obj.Type = FadeSpec.Obj

    def apply[S <: Sys[S]](key: String, value: FadeSpec.Obj[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new FadeSpecAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class FadeSpecAttribute[S <: Sys[S]](val key: String, val objH: stm.Source[S#Tx, FadeSpec.Obj[S]])
                                                    (implicit val context: AuralContext[S])
    extends ExprImpl[S, FadeSpec] {

    def tpe: Obj.Type = FadeSpec.Obj

    def preferredNumChannels(implicit tx: S#Tx): Int = 4

    def mkValue(timeRef: TimeRef, spec: FadeSpec)(implicit tx: S#Tx): Scalar = {
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

  private[this] object DoubleVectorAttribute extends Factory with StartLevelViewFactory {
    type Repr[S <: stm.Sys[S]] = DoubleVector[S]

    def tpe: Obj.Type = DoubleVector

    def apply[S <: Sys[S]](key: String, value: DoubleVector[S], observer: Observer[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleVectorAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[S <: Sys[S]](value: Repr[S])(implicit tx: S#Tx): ControlValuesView[S] =
      new DoubleVectorStartLevel(tx.newHandle(value))
  }

  private final class DoubleVectorStartLevel[S <: Sys[S]](obj: stm.Source[S#Tx, DoubleVector[S]])
    extends NumericExprStartLevel(obj) {
    
    def mkValue(in: Vec[Double]): ControlValues = in.map(_.toFloat)
  }
  
  private final class DoubleVectorAttribute[S <: Sys[S]](val key: String, val objH: stm.Source[S#Tx, DoubleVector[S]])
                                                        (implicit val context: AuralContext[S])
    extends ExprImpl[S, Vec[Double]] with NumericExprImpl[S, Vec[Double]] {

    def tpe: Obj.Type = DoubleVector

    def preferredNumChannels(implicit tx: S#Tx): Int = objH().value.size

    def mkValue(in: Vec[Double]): Scalar = in.map(_.toFloat)

    override def toString = s"DoubleVectorAttribute($key)@${hashCode.toHexString}"
  }

  // ------------------- generic (dummy) -------------------

  private final class DummyAttribute[S <: Sys[S]](val key: String, val objH: stm.Source[S#Tx, Obj[S]])
    extends AuralAttribute[S] with DummyObservableImpl[S] {

    def tpe: Obj.Type = throw new UnsupportedOperationException("DummyAttribute.tpe")

    def preferredNumChannels(implicit tx: S#Tx): Int = 0

    def targetOption(implicit tx: S#Tx): Option[Target[S]] = None

    def state(implicit tx: S#Tx): State = Stopped

    def prepare(timeRef: TimeRef.Option, attr: Runner.Attr)(implicit tx: S#Tx): Unit = ()

    def run(timeRef: TimeRef.Option, target: Target[S])(implicit tx: S#Tx): Unit = ()

    def stop   ()(implicit tx: S#Tx): Unit = ()
    def dispose()(implicit tx: S#Tx): Unit = ()

//    def react(fun: (S#Tx) => (State) => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = DummyObservableImpl

    override def toString = s"DummyAttribute($key)@${hashCode.toHexString}"
  }
}
trait AuralAttributeImpl[S <: Sys[S]] extends AuralAttribute[S] with ObservableImpl[S, Runner.State] {
  import TxnLike.peer

  private[this] final val stateRef = Ref[State](Stopped)

  final def state(implicit tx: S#Tx): State = stateRef()
  final protected def state_=(value: State)(implicit tx: S#Tx): Unit = {
    val prev = stateRef.swap(value)
    if (value != prev) fire(value)
  }
}