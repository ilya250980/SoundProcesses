/*
 *  AuralAttributeImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.impl.DummyObservableImpl
import de.sciss.lucre.expr.graph.{Obj => ExObj}
import de.sciss.lucre.{AnyTxn, BooleanObj, Disposable, DoubleObj, DoubleVector, Expr, ExprLike, Folder, IExpr, IntObj, IntVector, Obj, Source, Txn, synth}
import de.sciss.synth.Curve
import de.sciss.proc.AuralAttribute.{Factory, Observer, Scalar, Target}
import de.sciss.proc.Runner.{Prepared, Running, State, Stopped}
import de.sciss.proc.{AuralAttribute, AuralContext, ControlValuesView, EnvSegment, FadeSpec, Grapheme, Proc, StartLevelViewFactory, TimeRef, Timeline}
import de.sciss.synth.ugen.ControlValues

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.concurrent.stm.Ref

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  import de.sciss.lucre.Txn.peer

  // ---- Factory ----

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[T <: synth.Txn[T]](key: String, value: Obj[T], observer: Observer[T])
                              (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] = {
    val tid = value.tpe.typeId
    map.get(tid) match {
      case Some(factory) =>
        factory(key, value.asInstanceOf[factory.Repr[T]], observer)
      case None =>
        Console.err.println(s"Warning: AuralAttribute - no factory for ${value.tpe}")
        new DummyAttribute[T](key, tx.newHandle(value))
    }
  }

  def expr[T <: synth.Txn[T], A](key: String, value: IExpr[T, A], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] = {
    val v = value.value

    def fallBack(): AuralAttribute[T] = {
      Console.err.println(s"Warning: AuralAttribute - no factory for $value")
      new DummyExprLike(key, value)
    }

    v match {
      case _: Int       => IntExprLike      (key, value.asInstanceOf[ExprLike[T, Int      ]], observer)
      case _: Double    => DoubleExprLike   (key, value.asInstanceOf[ExprLike[T, Double   ]], observer)
      case _: Boolean   => BooleanExprLike  (key, value.asInstanceOf[ExprLike[T, Boolean  ]], observer)
      case _: FadeSpec  => FadeSpecExprLike (key, value.asInstanceOf[ExprLike[T, FadeSpec ]], observer)
      case sq: ISeq[_] if sq.forall(_.isInstanceOf[Double]) =>
        DoubleVectorExprLike (key, value.asInstanceOf[ExprLike[T, ISeq[Double]]], observer)
      case sq: ISeq[_] if sq.forall(_.isInstanceOf[Int]) =>
        IntVectorExprLike    (key, value.asInstanceOf[ExprLike[T, ISeq[Int]   ]], observer)
      case obj: ExObj   =>
        // XXX TODO: track changes to expression
        obj.peer[T] match {
          case Some(p)  => AuralAttribute(key, value = p, observer = observer)
          case None     => fallBack()
        }
      case _ => fallBack()
    }
  }

  private[this] var map = Map[Int, Factory](
    IntObj      .typeId -> IntAttribute,
    DoubleObj   .typeId -> DoubleAttribute,
    BooleanObj  .typeId -> BooleanAttribute,
    FadeSpec    .typeId -> FadeSpecAttribute,
    DoubleVector.typeId -> DoubleVectorAttribute,
    IntVector   .typeId -> IntVectorAttribute,
    Grapheme    .typeId -> AuralGraphemeAttribute,
    Proc.Output .typeId -> AuralOutputAttribute,
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
    IntVector   .typeId -> IntVectorAttribute,
//    Grapheme    .typeId -> AuralGraphemeAttribute,
    EnvSegment  .typeId -> AuralEnvSegmentAttribute
  )

  def addStartLevelViewFactory(f: StartLevelViewFactory): Unit = sync.synchronized {
    val tid = f.tpe.typeId
    if (startLevelMap.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    startLevelMap += tid -> f
  }

  def startLevelView[T <: synth.Txn[T]](obj: Obj[T])(implicit tx: T): ControlValuesView[T] = {
    val tid = obj.tpe.typeId
    startLevelMap.get(tid) match {
      case Some(factory) =>
        factory.mkStartLevelView(obj.asInstanceOf[factory.Repr[T]])
      case None =>
        DummyScalarOptionView.asInstanceOf[ControlValuesView[T]]
    }
  }

  private object DummyScalarOptionView extends ControlValuesView[AnyTxn] with DummyObservableImpl[AnyTxn] {
    def apply()(implicit tx: AnyTxn): Option[ControlValues] = None
  }

  // ---- ----

  private final class Playing[T <: Txn[T]](val timeRef: TimeRef, val wallClock: Long,
                                           val target: Target[T], val value: AuralAttribute.Value) {
    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    def updateValue(newValue: AuralAttribute.Value): Playing[T] =
      new Playing(timeRef, wallClock, target, newValue)
  }

  trait ExprImpl[T <: synth.Txn[T], A]
    extends AuralAttributeImpl[T] { attr =>

    // ---- abstract ----

    protected val context: AuralContext[T]

    type Repr <: ExprLike[T, A]

    /** Creates a value representation of this input. If the value is a `Stream`,
      * that stream's node reference will be disposed when the input stops (is replaced by other input).
      */
    protected def mkValue(timeRef: TimeRef, in: A)(implicit tx: T): AuralAttribute.Value

    // ---- impl ----

//    import context.{scheduler => sched}
    import context.universe.{scheduler => sched}

    private[this] var obs: Disposable[T] = _
    private[this] val playRef = Ref(Option.empty[Playing[T]])

    final def targetOption(implicit tx: T): Option[Target[T]] = playRef().map(_.target)

    final def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit =
      state = Prepared

    final def run(timeRef: TimeRef.Option, target: Target[T])(implicit tx: T): Unit /* Instance */ = {
      state = Running
      val timeF   = timeRef.force
      val value   = updateTarget(timeF, target, obj.value)
      val playing = new Playing(timeF, sched.time, target, value)
      require(playRef.swap(Some(playing))(tx.peer).isEmpty)
    }

    final def stop()(implicit tx: T): Unit = {
      stopNoFire()
      state = Stopped
    }

    private def updateTarget(timeRef: TimeRef, target: Target[T], value: A)
                            (implicit tx: T): AuralAttribute.Value = {
      val ctlVal = mkValue(timeRef, value)
      target.put(this, ctlVal)
      ctlVal
    }

    final protected def valueChanged(value: A)(implicit tx: T): Unit = {
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

    def init(expr: ExprLike[T, A])(implicit tx: T): this.type = {
      obs = expr.changed.react { implicit tx => change =>
        valueChanged(change.now)
      }
      this
    }

    @inline
    private def freeValue(p: Playing[T])(implicit tx: T): Unit = p.value match {
      case AuralAttribute.Stream(nodeRef, _) => nodeRef.node.dispose()
      case _ =>
    }

    private def stopNoFire()(implicit tx: T): Unit =
      playRef.swap(None).foreach { p =>
        p.target.remove(this)
        freeValue(p)
      }

    def dispose()(implicit tx: T): Unit = {
      obs.dispose()
      stopNoFire()
    }
  }

  private trait SingleChannelImpl[T <: synth.Txn[T], A] extends ExprImpl[T, A] {
    final def preferredNumChannels(implicit tx: T): Int = 1
  }

  private abstract class NumericExprStartLevel[T <: Txn[T], A](obj: Source[T, Expr[T, A]])
    extends ControlValuesView[T] {

    def mkValue(in: A): ControlValues

    def apply()(implicit tx: T): Option[ControlValues] = Some(mkValue(obj().value))

    def react(fun: T => Option[ControlValues] => Unit)(implicit tx: T): Disposable[T] =
      obj().changed.react { implicit tx => ch =>
        val lvlCh = ch.map(mkValue)
        if (lvlCh.isSignificant) fun(tx)(Some(lvlCh.now))
      }
  }

  private trait NumericExprImpl[T <: synth.Txn[T], A] extends ExprImpl[T, A] {
    def mkValue(in: A): AuralAttribute.Scalar

    final def mkValue(timeRef: TimeRef, value: A)(implicit tx: T): AuralAttribute.Value = mkValue(value)
  }

  // ------------------- IntObj -------------------

  private[this] object IntAttribute extends Factory with StartLevelViewFactory {
    type Repr[T <: Txn[T]] = IntObj[T]

    def tpe: Obj.Type = IntObj

    def apply[T <: synth.Txn[T]](key: String, value: IntObj[T], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new IntAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[T <: Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] =
      new IntStartLevel[T](tx.newHandle(value))
  }

  private final class IntStartLevel[T <: Txn[T]](obj: Source[T, IntObj[T]])
    extends NumericExprStartLevel[T, Int](obj) {

    def mkValue(in: Int): ControlValues = in.toFloat
  }
  
  private final class IntAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, IntObj[T]])
                                               (implicit val context: AuralContext[T])
    extends SingleChannelImpl[T, Int] with NumericExprImpl[T, Int] {

    // def tpe: Obj.Type = IntObj

    type Repr = IntObj[T]

    def mkValue(in: Int): Scalar = in.toFloat

    override def obj(implicit tx: T): Repr = objH()

    override def toString = s"IntAttribute($key)@${hashCode.toHexString}"
  }

  private[this] object IntExprLike {
    def apply[T <: synth.Txn[T]](key: String, value: ExprLike[T, Int], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new IntExprLike(key, value).init(value)
  }

  private final class IntExprLike[T <: synth.Txn[T]](val key: String, _obj: ExprLike[T, Int])
                                              (implicit val context: AuralContext[T])
    extends SingleChannelImpl[T, Int] with NumericExprImpl[T, Int] {

    type Repr = ExprLike[T, Int]

    def mkValue(in: Int): Scalar = in.toFloat

    override def obj(implicit tx: T): Repr = _obj

    override def toString = s"IntExprLike($key)@${hashCode.toHexString}"
  }

  // ------------------- DoubleObj ------------------- 

  private[this] object DoubleAttribute extends Factory with StartLevelViewFactory  {
    type Repr[T <: Txn[T]] = DoubleObj[T]

    def tpe: Obj.Type = DoubleObj

    def apply[T <: synth.Txn[T]](key: String, value: DoubleObj[T], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new DoubleAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[T <: Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] =
      new DoubleStartLevel[T](tx.newHandle(value))
  }

  private final class DoubleStartLevel[T <: Txn[T]](obj: Source[T, DoubleObj[T]])
    extends NumericExprStartLevel[T, Double](obj) {

    def mkValue(in: Double): ControlValues = in
  }
  
  private final class DoubleAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, DoubleObj[T]])
                                                  (implicit val context: AuralContext[T])
    extends SingleChannelImpl[T, Double] with NumericExprImpl[T, Double] {

    // def tpe: Obj.Type = DoubleObj

    type Repr = DoubleObj[T]

    def mkValue(value: Double): Scalar = value.toFloat

    def obj(implicit tx: T): Repr = objH()

    override def toString = s"DoubleAttribute($key)@${hashCode.toHexString}"
  }

  private[this] object DoubleExprLike {
    def apply[T <: synth.Txn[T]](key: String, value: ExprLike[T, Double], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new DoubleExprLike(key, value).init(value)
  }

  private final class DoubleExprLike[T <: synth.Txn[T]](val key: String, _obj: ExprLike[T, Double])
                                              (implicit val context: AuralContext[T])
    extends SingleChannelImpl[T, Double] with NumericExprImpl[T, Double] {

    type Repr = ExprLike[T, Double]

    def mkValue(in: Double): Scalar = in.toFloat

    override def obj(implicit tx: T): Repr = _obj

    override def toString = s"DoubleExprLike($key)@${hashCode.toHexString}"
  }

  // ------------------- BooleanObj ------------------- 

  private[this] object BooleanAttribute extends Factory with StartLevelViewFactory {
    type Repr[T <: Txn[T]] = BooleanObj[T]

    def tpe: Obj.Type = BooleanObj

    def apply[T <: synth.Txn[T]](key: String, value: BooleanObj[T], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new BooleanAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[T <: Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] =
      new BooleanStartLevel[T](tx.newHandle(value))
  }

  private final class BooleanStartLevel[T <: Txn[T]](obj: Source[T, BooleanObj[T]])
    extends NumericExprStartLevel[T, Boolean](obj) {

    def mkValue(in: Boolean): ControlValues = if (in) 1f else 0f
  }
  
  private final class BooleanAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, BooleanObj[T]])
                                                   (implicit val context: AuralContext[T])
    extends SingleChannelImpl[T, Boolean] with NumericExprImpl[T, Boolean] {

    // def tpe: Obj.Type = BooleanObj

    type Repr = BooleanObj[T]

    def mkValue(in: Boolean): Scalar = if (in) 1f else 0f

    def obj(implicit tx: T): Repr = objH()

    override def toString = s"BooleanAttribute($key)@${hashCode.toHexString}"
  }

  private[this] object BooleanExprLike {
    def apply[T <: synth.Txn[T]](key: String, value: ExprLike[T, Boolean], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new BooleanExprLike(key, value).init(value)
  }

  private final class BooleanExprLike[T <: synth.Txn[T]](val key: String, _obj: ExprLike[T, Boolean])
                                                 (implicit val context: AuralContext[T])
    extends SingleChannelImpl[T, Boolean] with NumericExprImpl[T, Boolean] {

    type Repr = ExprLike[T, Boolean]

    def mkValue(in: Boolean): Scalar = if (in) 1f else 0f

    override def obj(implicit tx: T): Repr = _obj

    override def toString = s"BooleanExprLike($key)@${hashCode.toHexString}"
  }

  // ------------------- FadeSpec.Obj ------------------- 

  private[this] object FadeSpecAttribute extends Factory {
    type Repr[T <: Txn[T]] = FadeSpec.Obj[T]

    def tpe: Obj.Type = FadeSpec.Obj

    def apply[T <: synth.Txn[T]](key: String, value: FadeSpec.Obj[T], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new FadeSpecAttribute(key, tx.newHandle(value)).init(value)
  }
  private final class FadeSpecAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, FadeSpec.Obj[T]])
                                                    (implicit val context: AuralContext[T])
    extends ExprImpl[T, FadeSpec] {

    // def tpe: Obj.Type = FadeSpec.Obj

    type Repr = FadeSpec.Obj[T]

    def preferredNumChannels(implicit tx: T): Int = 4

    def mkValue(timeRef: TimeRef, spec: FadeSpec)(implicit tx: T): Scalar = {
      val v = Vector[Float](
        (spec.numFrames / TimeRef.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
          case Curve.parametric(c)  => c
          case _                    => 0f
        }, spec.floor
      )
      v
    }

    def obj(implicit tx: T): Repr = objH()

    override def toString = s"FadeSpecAttribute($key)@${hashCode.toHexString}"
  }

  private[this] object FadeSpecExprLike {
    def apply[T <: synth.Txn[T]](key: String, value: ExprLike[T, FadeSpec], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new FadeSpecExprLike(key, value).init(value)
  }

  private final class FadeSpecExprLike[T <: synth.Txn[T]](val key: String, _obj: ExprLike[T, FadeSpec])
                                                   (implicit val context: AuralContext[T])
    extends ExprImpl[T, FadeSpec] {

    // def tpe: Obj.Type = FadeSpec.Obj

    type Repr = ExprLike[T, FadeSpec]

    def preferredNumChannels(implicit tx: T): Int = 4

    def mkValue(timeRef: TimeRef, spec: FadeSpec)(implicit tx: T): Scalar = {
      val v = Vector[Float](
        (spec.numFrames / TimeRef.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
          case Curve.parametric(c)  => c
          case _                    => 0f
        }, spec.floor
      )
      v
    }

    def obj(implicit tx: T): Repr = _obj

    override def toString = s"FadeSpecExprLike($key)@${hashCode.toHexString}"
  }

  // ------------------- DoubleVector ------------------- 

  private[this] object DoubleVectorAttribute extends Factory with StartLevelViewFactory {
    type Repr[T <: Txn[T]] = DoubleVector[T]

    def tpe: Obj.Type = DoubleVector

    def apply[T <: synth.Txn[T]](key: String, value: DoubleVector[T], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new DoubleVectorAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[T <: Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] =
      new DoubleVectorStartLevel[T](tx.newHandle(value))
  }

  private final class DoubleVectorStartLevel[T <: Txn[T]](obj: Source[T, DoubleVector[T]])
    extends NumericExprStartLevel[T, Vec[Double]](obj) {
    
    def mkValue(in: Vec[Double]): ControlValues = in.map(_.toFloat)
  }
  
  private final class DoubleVectorAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, DoubleVector[T]])
                                                        (implicit val context: AuralContext[T])
    extends ExprImpl[T, Vec[Double]] with NumericExprImpl[T, Vec[Double]] {

    // def tpe: Obj.Type = DoubleVector

    type Repr = DoubleVector[T]

    def preferredNumChannels(implicit tx: T): Int = objH().value.size

    def mkValue(in: Vec[Double]): Scalar = in.map(_.toFloat)

    def obj(implicit tx: T): Repr = objH()

    override def toString = s"DoubleVectorAttribute($key)@${hashCode.toHexString}"
  }

  private[this] object DoubleVectorExprLike {
    def apply[T <: synth.Txn[T]](key: String, value: ExprLike[T, ISeq[Double]], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new DoubleVectorExprLike(key, value).init(value)
  }

  private final class DoubleVectorExprLike[T <: synth.Txn[T]](val key: String, _obj: ExprLike[T, ISeq[Double]])
                                                        (implicit val context: AuralContext[T])
    extends ExprImpl[T, ISeq[Double]] with NumericExprImpl[T, ISeq[Double]] {

    // def tpe: Obj.Type = DoubleVector

    type Repr = ExprLike[T, ISeq[Double]]

    def preferredNumChannels(implicit tx: T): Int = mkValue0(_obj.value).size

    def mkValue(in: ISeq[Double]): Scalar = mkValue0(in)

    // no element type to avoid runtime class-cast-exceptions
    private def mkValue0(in: ISeq[_]): Vec[Float] = in.iterator.collect {
      case d: Double => d.toFloat
    } .toIndexedSeq

    def obj(implicit tx: T): Repr = _obj

    override def toString = s"DoubleVectorExprLike($key)@${hashCode.toHexString}"
  }

  // ------------------- IntVector ------------------- 

  private[this] object IntVectorAttribute extends Factory with StartLevelViewFactory {
    type Repr[T <: Txn[T]] = IntVector[T]

    def tpe: Obj.Type = IntVector

    def apply[T <: synth.Txn[T]](key: String, value: IntVector[T], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new IntVectorAttribute(key, tx.newHandle(value)).init(value)

    def mkStartLevelView[T <: Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] =
      new IntVectorStartLevel[T](tx.newHandle(value))
  }

  private final class IntVectorStartLevel[T <: Txn[T]](obj: Source[T, IntVector[T]])
    extends NumericExprStartLevel[T, Vec[Int]](obj) {

    def mkValue(in: Vec[Int]): ControlValues = in.map(_.toFloat)
  }

  private final class IntVectorAttribute[T <: synth.Txn[T]](val key: String, objH: Source[T, IntVector[T]])
                                                        (implicit val context: AuralContext[T])
    extends ExprImpl[T, Vec[Int]] with NumericExprImpl[T, Vec[Int]] {

    // def tpe: Obj.Type = IntVector

    type Repr = IntVector[T]

    def preferredNumChannels(implicit tx: T): Int = objH().value.size

    def mkValue(in: Vec[Int]): Scalar = in.map(_.toFloat)

    def obj(implicit tx: T): Repr = objH()

    override def toString = s"IntVectorAttribute($key)@${hashCode.toHexString}"
  }

  private[this] object IntVectorExprLike {
    def apply[T <: synth.Txn[T]](key: String, value: ExprLike[T, ISeq[Int]], observer: Observer[T])
                          (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
      new IntVectorExprLike(key, value).init(value)
  }

  private final class IntVectorExprLike[T <: synth.Txn[T]](val key: String, _obj: ExprLike[T, ISeq[Int]])
                                                       (implicit val context: AuralContext[T])
    extends ExprImpl[T, ISeq[Int]] with NumericExprImpl[T, ISeq[Int]] {

    // def tpe: Obj.Type = IntVector

    type Repr = ExprLike[T, ISeq[Int]]

    def preferredNumChannels(implicit tx: T): Int = mkValue0(_obj.value).size

    def mkValue(in: ISeq[Int]): Scalar = mkValue0(in)

    // no element type to avoid runtime class-cast-exceptions
    private def mkValue0(in: ISeq[_]): Vec[Float] = in.iterator.collect {
      case d: Int => d.toFloat
    } .toIndexedSeq

    def obj(implicit tx: T): Repr = _obj

    override def toString = s"IntVectorExprLike($key)@${hashCode.toHexString}"
  }

  // ------------------- generic (dummy) -------------------

  private abstract class DummyBase[T <: Txn[T]](val key: String)
    extends AuralAttribute[T] with DummyObservableImpl[T] {

    // final def tpe: Obj.Type = throw new UnsupportedOperationException("DummyAttribute.tpe")

    final def preferredNumChannels(implicit tx: T): Int = 0

    final def targetOption(implicit tx: T): Option[Target[T]] = None

    final def state(implicit tx: T): State = Stopped

    final def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = ()

    final def run(timeRef: TimeRef.Option, target: Target[T])(implicit tx: T): Unit = ()
    
    final def stop   ()(implicit tx: T): Unit = ()
    final def dispose()(implicit tx: T): Unit = ()
  }

  private final class DummyAttribute[T <: Txn[T]](key: String, objH: Source[T, Obj[T]])
    extends DummyBase[T](key) {

    type Repr = Obj[T]

    def obj(implicit tx: T): Repr = objH()

    override def toString = s"DummyAttribute($key)@${hashCode.toHexString}"
  }
  
  private final class DummyExprLike[T <: Txn[T], A](key: String, _obj: ExprLike[T, A])
    extends DummyBase[T](key) {

    type Repr = ExprLike[T, A]

    def obj(implicit tx: T): Repr = _obj

    override def toString = s"DummyExprLike($key)@${hashCode.toHexString}"
  }
}
trait AuralAttributeImpl[T <: Txn[T]] extends AuralAttribute[T] with BasicViewBaseImpl[T]