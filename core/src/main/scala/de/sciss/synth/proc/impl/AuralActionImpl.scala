/*
 *  AuralActionImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.{Action, AuralContext, AuralObj, Runner, TimeRef, Universe}

object AuralActionImpl extends AuralObj.Factory {
  type Repr[S <: Sys[S]]  = Action[S]
  def tpe: Obj.Type       = Action

  def apply[S <: SSys[S]](obj: Action[S], attr: Runner.Attr[S])
                         (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH, attr)
  }

  private final class Impl[S <: SSys[S]](objH: stm.Source[S#Tx, Action[S]], attr: Runner.Attr[S])
                                        (implicit context: AuralContext[S])
    extends ActionRunnerImpl.Base[S, Unit] with AuralObj.Action[S] {

    implicit def universe: Universe[S] = context.universe

    override type Repr = Action[S]

    override def obj(implicit tx: S#Tx): Action[S] = objH()

    private def invokeValue(implicit tx: S#Tx): Any =
      attr.get("value").getOrElse(())

    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit =
      execute(invokeValue = invokeValue)

    override def toString = s"AuralAction@${hashCode().toHexString}"

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}