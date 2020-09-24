/*
 *  AuralEnvSegmentAttribute.scala
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

package de.sciss.synth.proc.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.synth.{Bus, Synth}
import de.sciss.lucre.{BiPin, Disposable, Obj, Source, Txn, synth}
import de.sciss.synth.proc.AuralAttribute.{Factory, GraphemeAware, Observer}
import de.sciss.synth.proc.graph.{Duration, Offset}
import de.sciss.synth.proc.impl.AuralAttributeImpl.ExprImpl
import de.sciss.synth.proc.{AuralAttribute, AuralContext, ControlValuesView, EnvSegment, Grapheme, StartLevelViewFactory, TimeRef}
import de.sciss.synth.ugen.ControlValues
import de.sciss.synth.{Curve, SynthGraph, addToHead, ugen, ControlSet => CS}

import scala.concurrent.stm.Ref

object AuralEnvSegmentAttribute extends Factory with StartLevelViewFactory {
  type Repr[T <: Txn[T]] = EnvSegment.Obj[T]

  def tpe: Obj.Type = EnvSegment.Obj

  def apply[T <: synth.Txn[T]](key: String, value: Repr[T], observer: Observer[T])
                              (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
    new Impl(key, tx.newHandle(value)).init(value)

  def mkStartLevelView[T <: Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] =
    new StartLevelView(tx.newHandle(value))

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
    val cid         = curveCtl out 0
    val cvt         = curveCtl out 1
    val phase       = Line.ar(off, dur, dur - off)
    val curve       = Env.Curve(id = cid, curvature = cvt)
    val env         = IEnv(startLevel, Env.Segment(dur = dur, targetLevel = endLevel, curve = curve) :: Nil)
    val sig         = IEnvGen.ar(env, phase)
    Out.ar(out, sig)
  }

  private final class StartLevelView[T <: Txn[T], A](obj: Source[T, Repr[T]])
    extends ControlValuesView[T] {

    private def levelOf(e: EnvSegment): ControlValues = e.startLevelsAsControl

    def apply()(implicit tx: T): Option[ControlValues] = Some(levelOf(obj().value))

    def react(fun: T => Option[ControlValues] => Unit)(implicit tx: T): Disposable[T] =
      obj().changed.react { implicit tx => ch =>
        val lvlCh = ch.map(levelOf)
        if (lvlCh.isSignificant) fun(tx)(Some(lvlCh.now))
      }
  }

  private final class SegmentEnd[T <: Txn[T]](val frame: Long, val view: ControlValuesView[T], obs: Disposable[T])
    extends Disposable[T] {

    def dispose()(implicit tx: T): Unit = obs.dispose()
  }

  private final class Impl[T <: synth.Txn[T]](val key: String, objH: Source[T, AuralEnvSegmentAttribute.Repr[T]])
                                             (implicit val context: AuralContext[T])
    extends ExprImpl[T, EnvSegment] with GraphemeAware[T] {

    private[this] val _endLevel = Ref(Option.empty[SegmentEnd[T]])
    private[this] val _grObs    = Ref(Disposable.empty[T])

    // def tpe: Obj.Type = EnvSegment.Obj

    type Repr = AuralEnvSegmentAttribute.Repr[T]

    def obj(implicit tx: T): EnvSegment.Obj[T] = objH()

    def preferredNumChannels(implicit tx: T): Int = objH().value.numChannels

    private def valueChanged()(implicit tx: T): Unit = valueChanged(objH().value)

    override def dispose()(implicit tx: T): Unit = {
      _endLevel.swap(None).foreach(_.dispose())
      _grObs().dispose()
      super.dispose()
    }

    private def setCeil(stopFrame: Long, ceilObj: Obj[T], fire: Boolean)(implicit tx: T): Unit = {
      val levelView = AuralAttribute.startLevelView(ceilObj)
      val obsLvl    = levelView.react { implicit tx => _ => valueChanged() }
      val se        = new SegmentEnd[T](stopFrame, levelView, obsLvl)
      val oldOpt    = _endLevel.swap(Some(se))
      oldOpt.foreach(_.dispose())
      if (fire) {
        val lvlNow    = levelView()
        val lvlBefore = oldOpt.flatMap(_.view())
        if (lvlNow !== lvlBefore) valueChanged()
      }
    }

    def setGrapheme(pos: Long, g: Grapheme[T])(implicit tx: T): Unit =
      if (pos < Long.MaxValue) setGrapheme(pos = pos, g = g, fire = false)

    private def setGrapheme(pos: Long, g: BiPin[T, Obj[T]], fire: Boolean)(implicit tx: T): Unit = {
      val stopFrame = g.ceil(pos + 1L).fold(Long.MinValue) { entry =>
        val _stopFrame = entry.key.value
        setCeil(_stopFrame, entry.value, fire = fire)
        _stopFrame
      }
      val obsG = g.changed.react { implicit tx => upd =>
        @inline def reset(): Unit = setGrapheme(pos, upd.pin, fire = fire)
        upd.changes.foreach {
          case Grapheme.Added   (time, _) if time       > pos && time      <= stopFrame  => reset()
          case Grapheme.Removed (time, _) if time       > pos && time      == stopFrame  => reset()
          case Grapheme.Moved   (ch  , _) if (ch.before > pos && ch.before == stopFrame) ||
                                             (ch.now    > pos && ch.now    <= stopFrame) => reset()
          case _ =>
        }
      }
      _grObs.swap(obsG).dispose()
    }

    protected def mkValue(timeRef: TimeRef, seg: EnvSegment)(implicit tx: T): AuralAttribute.Value = {
      _endLevel() match {
        case Some(se) if seg.curve !== Curve.step => se.view() match {
          case Some(scalar) => mkValueWithEnd   (seg, timeRef, endFrame = se.frame, endLevel = scalar)
          case _            => mkValueWithoutEnd(seg)
        }
        case _              => mkValueWithoutEnd(seg)
      }
    }

    private def mkValueWithEnd(seg: EnvSegment, timeRef: TimeRef, endFrame: Long, endLevel: ControlValues)
                              (implicit tx: T): AuralAttribute.Value = {
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
        case (EnvSegment.Single(startLevel, _), ControlValues(Seq(targetLevel))) =>
          proceed(("start" -> startLevel: CS) :: ("end" -> targetLevel: CS) :: Nil, 1)
        case _ =>
          val startLevels0  = seg.startLevels
          val targetLevels0 = endLevel.seq //.values
          val numChannels   = math.max(startLevels0.size, targetLevels0.size)
          val startLevels   = Vector.tabulate(numChannels)(ch => startLevels0 (ch % startLevels0 .size).toFloat)
          val targetLevels  = if (targetLevels0.size == numChannels) targetLevels0 else
            Vector.tabulate(numChannels)(ch => targetLevels0(ch % targetLevels0.size))
          proceed(("start" -> startLevels: CS) :: ("end" -> targetLevels: CS) :: Nil, numChannels)
      }
    }

    private def mkValueWithoutEnd(seg: EnvSegment) /* (implicit tx: T) */: AuralAttribute.Value =
      seg match {
        case EnvSegment.Single(level , _) => level       .toFloat
        case EnvSegment.Multi (levels, _) => levels.map(_.toFloat)
      }

    override def toString = s"EnvSegmentAttribute($key)@${hashCode.toHexString}"
  }
}