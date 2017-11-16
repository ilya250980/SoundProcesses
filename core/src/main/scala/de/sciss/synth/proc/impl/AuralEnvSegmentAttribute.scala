package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralAttribute.{Factory, Observer}
import de.sciss.synth.proc.impl.AuralAttributeImpl.ExprImpl

object AuralEnvSegmentAttribute extends Factory {
  type Repr[S <: stm.Sys[S]] = EnvSegment.Obj[S]

  def typeID: Int = EnvSegment.typeID

  def apply[S <: Sys[S]](key: String, value: EnvSegment.Obj[S], observer: Observer[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
    new Impl(key, tx.newHandle(value)).init(value)

  private final class Impl[S <: Sys[S]](val key: String, val obj: stm.Source[S#Tx, Repr[S]])
    extends ExprImpl[S, EnvSegment] {

    def typeID: Int = EnvSegment.typeID

    def preferredNumChannels(implicit tx: S#Tx): Int = ???

    protected def mkValue(seg: EnvSegment): AuralAttribute.Value = ???

    override def toString = s"EnvSegmentAttribute($key)@${hashCode.toHexString}"
  }
}