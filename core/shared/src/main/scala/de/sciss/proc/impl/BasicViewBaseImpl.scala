/*
 *  BasicViewBaseImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.Txn
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.Txn.peer
import de.sciss.proc.{Runner, ViewBase}

import scala.concurrent.stm.Ref

trait BasicViewBaseImpl[T <: Txn[T]]
  extends ViewBase[T] with ObservableImpl[T, Runner.State] {

//  implicit final def workspace : WorkspaceHandle[T] = handler.workspace
//  implicit final def cursor    : Cursor[T]          = handler.cursor

  private[this] val stateRef = Ref[Runner.State](Runner.Stopped)

  final def state(implicit tx: T): Runner.State = stateRef()

  final protected def state_=(now: Runner.State)(implicit tx: T): Unit = {
    val before = stateRef.swap(now)
    if (before != now) {
      stateWillChanged(now)
      fire(now)
    }
  }

  protected def stateWillChanged(now: Runner.State)(implicit tx: T): Unit = ()
}