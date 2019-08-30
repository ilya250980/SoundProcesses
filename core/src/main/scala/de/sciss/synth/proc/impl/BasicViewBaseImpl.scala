/*
 *  BasicViewBaseImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.synth.proc.{Runner, ViewBase}

import scala.concurrent.stm.Ref

trait BasicViewBaseImpl[S <: Sys[S]]
  extends ViewBase[S] with ObservableImpl[S, Runner.State] {

//  implicit final def workspace : WorkspaceHandle[S] = handler.workspace
//  implicit final def cursor    : Cursor[S]          = handler.cursor

  private[this] val stateRef = Ref[Runner.State](Runner.Stopped)

  final def state(implicit tx: S#Tx): Runner.State = stateRef()

  final protected def state_=(now: Runner.State)(implicit tx: S#Tx): Unit = {
    val before = stateRef.swap(now)
    if (before != now) {
      stateWillChanged(now)
      fire(now)
    }
  }

  protected def stateWillChanged(now: Runner.State)(implicit tx: S#Tx): Unit = ()
}