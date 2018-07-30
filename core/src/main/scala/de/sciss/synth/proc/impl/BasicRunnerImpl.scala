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
import de.sciss.lucre.stm.{Cursor, Sys, WorkspaceHandle}

import scala.concurrent.stm.Ref

trait BasicRunnerImpl[S <: Sys[S]]
  extends BasicViewBaseImpl[S, Unit] with Runner[S] {

  implicit final def workspace : WorkspaceHandle[S] = handler.workspace
  implicit final def cursor    : Cursor[S]          = handler.cursor

  final object messages extends Runner.Messages[S#Tx] with ObservableImpl[S, List[Runner.Message]] {
    private[this] val ref = Ref(List.empty[Runner.Message])

    def current(implicit tx: S#Tx): List[Runner.Message] = ref()

    protected def current_=(value: List[Runner.Message])(implicit tx: S#Tx): Unit = {
      ref() = value
      fire(value)
    }
  }
}