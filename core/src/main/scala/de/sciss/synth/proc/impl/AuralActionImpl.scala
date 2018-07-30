/*
 *  AuralActionImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}

object AuralActionImpl extends AuralObj.Factory {
  type Repr[S <: Sys[S]]  = Action[S]
  def tpe: Obj.Type       = Action

  def apply[S <: SSys[S]](obj: Action[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH)
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Action[S]])(implicit context: AuralContext[S])
    extends ActionRunnerImpl.Base[S, Unit] with AuralObj.Action[S] {

    protected def workspace : WorkspaceHandle [S] = context.workspace
    protected def cursor    : Cursor          [S] = context.scheduler.cursor

    override def toString = s"AuralAction@${hashCode().toHexString}"
  }
}