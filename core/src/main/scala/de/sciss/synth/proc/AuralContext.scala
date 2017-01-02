/*
 *  AuralContext.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.synth.{Server, Sys => SSys}
import de.sciss.synth.proc.impl.{AuralContextImpl => Impl}

object AuralContext {
  def apply[S <: SSys[S]](server: Server, scheduler: Scheduler[S])
                        (implicit tx: S#Tx, workspaceHandle: WorkspaceHandle[S]): AuralContext[S] =
    Impl(server, scheduler)

  def apply[S <: SSys[S]](server: Server)(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                         workspaceHandle: WorkspaceHandle[S]): AuralContext[S] = {
    val sched = Scheduler[S]
    apply(server, sched)
  }
}
trait AuralContext[S <: Sys[S]] extends AuxContext[S] {
  def server: Server

  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A]

  val scheduler: Scheduler[S]

  implicit def workspaceHandle: WorkspaceHandle[S]
}