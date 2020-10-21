package de.sciss.synth.proc.impl

import de.sciss.lucre.{Obj, Txn}
import de.sciss.lucre.synth.{Txn => STxn}
import de.sciss.synth.proc.{AuralContext, AuralObj, Proc, Runner}

object AuralProcFactoryImpl extends AuralObj.Factory {
  type Repr[T <: Txn[T]] = Proc[T]

  private lazy val _init: Unit =
    AuralObj.addFactory(this)

  def init(): Unit = _init

  def tpe: Obj.Type = Proc

  def apply[T <: STxn[T]](obj: Proc[T], attr: Runner.Attr[T] = Runner.emptyAttr[T])
                         (implicit tx: T, context: AuralContext[T]): AuralObj.Proc[T] =
    AuralProcImpl(obj, attr)
}