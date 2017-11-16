package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer, ScalarOptionView}
import de.sciss.synth.proc.impl.AuralAttributeImpl.ExprImpl
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.synth.Curve
import de.sciss.equal.Implicits._

import scala.concurrent.stm.Ref

object AuralEnvSegmentAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = EnvSegment.Obj[S]

  def typeID: Int = EnvSegment.typeID

  def apply[S <: Sys[S]](key: String, value: EnvSegment.Obj[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new Impl(key, tx.newHandle(value)).init(value)

  private final class Impl[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Repr[S]])
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
      ???
    }

    private def mkValueWithoutEnd(seg: EnvSegment)(implicit tx: S#Tx): AuralAttribute.Value =
      seg match {
        case EnvSegment.Single(level , _) => level       .toFloat
        case EnvSegment.Multi (levels, _) => levels.map(_.toFloat)
      }

    override def toString = s"EnvSegmentAttribute($key)@${hashCode.toHexString}"
  }
}