package de.sciss.synth.proc
package impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.{Bus, Synth, Sys}
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, ScalarOptionView}
import de.sciss.synth.proc.impl.AuralAttributeImpl.ExprImpl
import de.sciss.synth.{Curve, SynthGraph, addToHead, ugen, ControlSet => CS}

import scala.concurrent.stm.Ref

object AuralEnvSegmentAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = EnvSegment.Obj[S]

  def typeID: Int = EnvSegment.typeID

  def apply[S <: Sys[S]](key: String, value: EnvSegment.Obj[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new Impl(key, tx.newHandle(value)).init(value)

  private def mkEnvSegGraph(numChannels: Int): SynthGraph =
    if      (numChannels == 1) envSegGraph1
    else if (numChannels == 1) envSegGraph2
    else mkEnvSegGraphF(numChannels)

  private lazy val envSegGraph1 = mkEnvSegGraphF(1)
  private lazy val envSegGraph2 = mkEnvSegGraphF(2)

  private def mkEnvSegGraphF(numChannels: Int): SynthGraph = SynthGraph {
    import de.sciss.synth.Ops.stringToControl
    import ugen._
    val startLevel  = "start".ir
    val endLevel    = "end"  .ir
    val dur         = "dur"  .ir
    val out         = "out"  .kr
    val cid         = "cid"  .ir
    val cvt         = "cvt"  .ir
    val curve       = Env.Curve(id = cid, curvature = cvt)
    val env         = Env(startLevel = startLevel,
      segments = Env.Segment(dur = dur, targetLevel = endLevel, curve = curve) :: Nil)
    val sig         = EnvGen.ar(env)
    Out.ar(out, sig)
  }

  private final class Impl[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Repr[S]])
                                       (implicit context: AuralContext[S])
    extends ExprImpl[S, EnvSegment] with AuralAttribute.EndLevelSink[S] {

    private[this] val _endLevel     = Ref(Option.empty[ScalarOptionView[S]])
    private[this] val _endLevelObs  = Ref(Disposable.empty[S#Tx])

    def typeID: Int = EnvSegment.typeID

    def preferredNumChannels(implicit tx: S#Tx): Int = obj().value.numChannels

    private def valueChanged()(implicit tx: S#Tx): Unit = valueChanged(obj().value)

    override def dispose()(implicit tx: S#Tx): Unit = {
      _endLevelObs().dispose()
      super.dispose()
    }

    def endLevel_=(levelView: ScalarOptionView[S])(implicit tx: S#Tx): Unit = {
      val obs = levelView.react { implicit tx => _ => valueChanged() }
      _endLevel() = Some(levelView)
      _endLevelObs.swap(obs).dispose()
      valueChanged()
    }

    protected def mkValue(seg: EnvSegment)(implicit tx: S#Tx): AuralAttribute.Value = {
      _endLevel() match {
        case Some(view) if seg.curve !== Curve.step => view() match {
          case Some(scalar) => mkValueWithEnd   (seg, scalar)
          case _            => mkValueWithoutEnd(seg)
        }
        case _              => mkValueWithoutEnd(seg)
      }
    }

    private def mkValueWithEnd(seg: EnvSegment, end: AuralAttribute.Scalar)
                              (implicit tx: S#Tx): AuralAttribute.Value = {
      import context.server

      def proceed(args0: List[CS], numChannels: Int): AuralAttribute.Value = {
        val bus   = Bus.tmpAudio(server, numChannels)
        val dur   = ??? : Float
        val args  = ("dur" -> dur: CS) :: args0
        val syn   = Synth.play(mkEnvSegGraph(numChannels), nameHint = Some("env"))(
          target = server, addAction = addToHead, args = args)
        syn.write(bus -> "out")
        AuralAttribute.Stream(syn, bus)
        mkValueWithoutEnd(seg)
      }

      (seg, end) match {
        case (EnvSegment.Single(startLevel, _), AuralAttribute.ScalarValue(targetLevel)) =>
          proceed(("start" -> startLevel: CS) :: ("end" -> targetLevel: CS) :: Nil, 1)
        case _ =>
          val startLevels0  = seg.startLevels
          val targetLevels0 = end.values
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