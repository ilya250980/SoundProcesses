package de.sciss.synth.proc
package impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.{Bus, Synth, Sys}
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, Scalar, ScalarOptionView, SegmentEndSink, StartLevelViewFactory}
import de.sciss.synth.proc.graph.{Duration, Offset}
import de.sciss.synth.proc.impl.AuralAttributeImpl.ExprImpl
import de.sciss.synth.{Curve, SynthGraph, addToHead, ugen, ControlSet => CS}

import scala.concurrent.stm.Ref

object AuralEnvSegmentAttribute extends Factory with StartLevelViewFactory {
  type Repr[S <: stm.Sys[S]] = EnvSegment.Obj[S]

  def typeID: Int = EnvSegment.typeID

  def apply[S <: Sys[S]](key: String, value: Repr[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new Impl(key, tx.newHandle(value)).init(value)

  def mkStartLevelView[S <: Sys[S]](value: Repr[S])(implicit tx: S#Tx): ScalarOptionView[S] = ???

  private def mkEnvSegGraph(numChannels: Int): SynthGraph =
    if      (numChannels == 1) envSegGraph1
    else if (numChannels == 1) envSegGraph2
    else mkEnvSegGraphF(numChannels)

  private lazy val envSegGraph1 = mkEnvSegGraphF(1)
  private lazy val envSegGraph2 = mkEnvSegGraphF(2)

  private def mkEnvSegGraphF(numChannels: Int): SynthGraph = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import ugen._
    val zeroes      = Vector.fill(numChannels)(0.0f): ControlValues
    val startLevel  = "start" .ir(zeroes)
    val endLevel    = "end"   .ir(zeroes)
    val off         = Offset  .ir
    val dur         = Duration.ir
    val out         = "out"   .kr
    val curveCtl    = "curve" .ir(Vector.fill(2)(0.0f))
    val cid         = curveCtl \ 0 // "cid"   .ir
    val cvt         = curveCtl \ 1 // "cvt"   .ir
    val phase       = Line.ar(off, dur, dur - off)
    val curve       = Env.Curve(id = cid, curvature = cvt)
    val env         = IEnv(startLevel, Env.Segment(dur = dur, targetLevel = endLevel, curve = curve) :: Nil)
    val sig         = IEnvGen.ar(env, phase)
    //    val env         = Env(startLevel = startLevel,
//      segments = Env.Segment(dur = dur, targetLevel = endLevel, curve = curve) :: Nil)
//    val sig         = EnvGen.ar(env)
    Out.ar(out, sig)
  }

  private final class SegmentEnd[S <: Sys[S]](val frame: Long, val view: ScalarOptionView[S], obs: Disposable[S#Tx])
    extends Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = obs.dispose()
  }

  private final class StartObserver[S <: Sys[S], A](view: Impl[S])
    extends ObservableImpl[S, Option[Scalar]] with ScalarOptionView[S] {

    def apply()(implicit tx: S#Tx): Option[Scalar] = Some(view.obj().value.startLevelsAsAttrScalar)
  }

  private final class Impl[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Repr[S]])
                                       (implicit val context: AuralContext[S])
    extends ExprImpl[S, EnvSegment] with SegmentEndSink[S] /* with StartLevelSource[S] */ {

    private[this] val _endLevel     = Ref(Option.empty[SegmentEnd[S]])

    def typeID: Int = EnvSegment.typeID

    def preferredNumChannels(implicit tx: S#Tx): Int = obj().value.numChannels

    private def valueChanged()(implicit tx: S#Tx): Unit = valueChanged(obj().value)

    override def dispose()(implicit tx: S#Tx): Unit = {
      _endLevel.swap(None).foreach(_.dispose())
      super.dispose()
    }

    def startLevel(implicit tx: S#Tx): ScalarOptionView[S] = new StartObserver(this)

    def segmentEnd_=(stopFrame: Long, levelView: ScalarOptionView[S])(implicit tx: S#Tx): Unit = {
      val obs = levelView.react { implicit tx => _ => valueChanged() }
      val se  = new SegmentEnd[S](stopFrame, levelView, obs)
      _endLevel.swap(Some(se)).foreach(_.dispose())
      valueChanged()
    }

    protected def mkValue(timeRef: TimeRef, seg: EnvSegment)(implicit tx: S#Tx): AuralAttribute.Value = {
      _endLevel() match {
        case Some(se) if seg.curve !== Curve.step => se.view() match {
          case Some(scalar) => mkValueWithEnd   (seg, timeRef, endFrame = se.frame, endLevel = scalar)
          case _            => mkValueWithoutEnd(seg)
        }
        case _              => mkValueWithoutEnd(seg)
      }
    }

    private def mkValueWithEnd(seg: EnvSegment, timeRef: TimeRef, endFrame: Long, endLevel: AuralAttribute.Scalar)
                              (implicit tx: S#Tx): AuralAttribute.Value = {
      import context.server

      def proceed(args0: List[CS], numChannels: Int): AuralAttribute.Value = {
        val bus   = Bus.tmpAudio(server, numChannels)
        val dur   = (endFrame - timeRef.span.start) / TimeRef.SampleRate
        val off   = timeRef.offset / TimeRef.SampleRate
        val curvature = seg.curve match {
          case Curve.parametric(f) => f
          case _ => 0f
        }
        val args  = (Duration.key -> dur: CS) :: (Offset.key -> off: CS) ::
          ("curve" -> Vector(seg.curve.id.toFloat, curvature): CS) :: args0
        val syn   = Synth.play(mkEnvSegGraph(numChannels), nameHint = Some("env"))(
          target = server, addAction = addToHead, args = args)
        syn.write(bus -> "out")
        AuralAttribute.Stream(syn, bus)
      }

      (seg, endLevel) match {
        case (EnvSegment.Single(startLevel, _), AuralAttribute.ScalarValue(targetLevel)) =>
          proceed(("start" -> startLevel: CS) :: ("end" -> targetLevel: CS) :: Nil, 1)
        case _ =>
          val startLevels0  = seg.startLevels
          val targetLevels0 = endLevel.values
          val numChannels   = math.max(startLevels0.size, targetLevels0.size)
          val startLevels   = Vector.tabulate(numChannels)(ch => startLevels0 (ch % startLevels0 .size).toFloat)
          val targetLevels  = if (targetLevels0.size == numChannels) targetLevels0 else
            Vector.tabulate(numChannels)(ch => targetLevels0(ch % targetLevels0.size))
          proceed(("start" -> startLevels: CS) :: ("end" -> targetLevels: CS) :: Nil, numChannels)
      }
    }

    private def mkValueWithoutEnd(seg: EnvSegment) /* (implicit tx: S#Tx) */: AuralAttribute.Value =
      seg match {
        case EnvSegment.Single(level , _) => level       .toFloat
        case EnvSegment.Multi (levels, _) => levels.map(_.toFloat)
      }

    override def toString = s"EnvSegmentAttribute($key)@${hashCode.toHexString}"
  }
}