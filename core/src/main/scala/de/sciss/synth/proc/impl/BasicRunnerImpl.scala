/*
 *  BasicRunnerImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, WorkspaceHandle}
import de.sciss.lucre.synth.Sys

import scala.concurrent.stm.Ref

trait BasicRunnerImpl[S <: Sys[S]]
  extends ViewBase[S, Unit] with ObservableImpl[S, Runner.State] with Runner[S] {

//  final def tpe: Obj.Type = factory.tpe

  implicit final def workspace : WorkspaceHandle[S] = handler.workspace
  implicit final def cursor    : Cursor[S]          = handler.cursor

  private[this] val stateRef = Ref[Runner.State](Runner.Stopped)

  final def state(implicit tx: S#Tx): Runner.State = stateRef()

  final protected def state_=(now: Runner.State)(implicit tx: S#Tx): Unit = {
    val before = stateRef.swap(now)
    if (before != now) fire(now)
  }
}