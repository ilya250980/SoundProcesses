/*
 *  DebugAct.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.stm.Sys

object DebugAct {
  private final class Expanded[S <: Sys[S]](body: () => Unit) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit =
      tx.afterCommit(body())
  }
}

/** A debugging graph element wrapping an action function.
  * '''Warning:''' Cannot be serialized.
  *
  * @param body   the code to execute when the action is triggered.
  *               Note that this will be executed outside of the transactional context.
  */
final case class DebugAct(body: () => Unit) extends Act {
  type Repr[S <: Sys[S]] = IAction[S]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
    new DebugAct.Expanded(body)
}