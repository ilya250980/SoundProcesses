/*
 *  AuralContext.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.synth.{Server, Sys => SSys}
import de.sciss.synth.proc.impl.{AuralContextImpl => Impl}

object AuralContext {
  def apply[S <: SSys[S]](server: Server)(implicit tx: S#Tx, universe: Universe[S]): AuralContext[S] =
    Impl(server)

//  def apply[S <: SSys[S]](server: Server)(implicit tx: S#Tx, cursor: stm.Cursor[S],
//                                          workspace: WorkspaceHandle[S]): AuralContext[S] = {
//    val scheduler = Scheduler[S]
//    apply(server, scheduler)
//  }
}
trait AuralContext[S <: Sys[S]] extends AuxContext[S] {
  def server: Server

  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A]

  implicit val universe: Universe[S]
}