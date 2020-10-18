///*
// *  AuralActionRawImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc.impl
//
//import de.sciss.synth.proc.{AuralContext, AuralObj, Runner, TimeRef, Universe}
//
//object AuralActionRawImpl extends AuralObj.Factory {
//  type Repr[T <: Txn[T]]  = ActionRaw[T]
//  def tpe: Obj.Type       = ActionRaw
//
//  def apply[T <: synth.Txn[T]](obj: ActionRaw[T], attr: Runner.Attr[T])
//                         (implicit tx: T, context: AuralContext[T]): AuralObj.ActionRaw[T] = {
//    val objH = tx.newHandle(obj)
//    new Impl(objH, attr)
//  }
//
//  private final class Impl[T <: synth.Txn[T]](objH: stm.Source[T, ActionRaw[T]], attr: Runner.Attr[T])
//                                        (implicit context: AuralContext[T])
//    extends ActionRawRunnerImpl.Base[T, Unit] with AuralObj.ActionRaw[T] {
//
//    implicit def universe: Universe[T] = context.universe
//
//    override type Repr = ActionRaw[T]
//
//    override def obj(implicit tx: T): ActionRaw[T] = objH()
//
//    private def invokeValue(implicit tx: T): Any =
//      attr.get("value").getOrElse(())
//
//    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: T): Unit =
//      execute(invokeValue = invokeValue)
//
//    override def toString = s"AuralActionRaw@${hashCode().toHexString}"
//
//    def dispose()(implicit tx: T): Unit = ()
//  }
//}