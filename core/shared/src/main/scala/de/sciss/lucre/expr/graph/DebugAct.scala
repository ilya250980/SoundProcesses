/*
 *  DebugAct.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.Txn
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction}

object DebugAct {
  private final class Expanded[T <: Txn[T]](body: () => Unit) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
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
  type Repr[T <: Txn[T]] = IAction[T]

  protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
    new DebugAct.Expanded(body)
}