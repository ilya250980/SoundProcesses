/*
 *  AuralActionImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.{ActionRaw, AuralContext, AuralObj, Runner, TimeRef, Universe}

object AuralActionImpl extends AuralObj.Factory {
  type Repr[S <: Sys[S]]  = ActionRaw[S]
  def tpe: Obj.Type       = ActionRaw

  def apply[S <: SSys[S]](obj: ActionRaw[S], attr: Runner.Attr[S])
                         (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.ActionRaw[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH, attr)
  }

  private final class Impl[S <: SSys[S]](objH: stm.Source[S#Tx, ActionRaw[S]], attr: Runner.Attr[S])
                                        (implicit context: AuralContext[S])
    extends ActionRawRunnerImpl.Base[S, Unit] with AuralObj.ActionRaw[S] {

    implicit def universe: Universe[S] = context.universe

    override type Repr = ActionRaw[S]

    override def obj(implicit tx: S#Tx): ActionRaw[S] = objH()

    private def invokeValue(implicit tx: S#Tx): Any =
      attr.get("value").getOrElse(())

    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit =
      execute(invokeValue = invokeValue)

    override def toString = s"AuralAction@${hashCode().toHexString}"

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}