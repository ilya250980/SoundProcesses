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

import de.sciss.lucre.synth.Server
import de.sciss.lucre.{Disposable, Obj, Txn, synth}
import de.sciss.synth.proc.impl.{AuralContextImpl => Impl}

object AuralContext {
  def apply[T <: synth.Txn[T]](server: Server)(implicit tx: T, universe: Universe[T]): AuralContext[T] =
    Impl(server)

//  def apply[T <: synth.Txn[T]](server: Server)(implicit tx: T, cursor: Cursor[T],
//                                          workspace: WorkspaceHandle[T]): AuralContext[T] = {
//    val scheduler = Scheduler[T]
//    apply(server, scheduler)
//  }
}
trait AuralContext[T <: Txn[T]] extends AuxContext[T] {
  def server: Server

  def acquire[A <: Disposable[T]](obj: Obj[T])(init: => A)(implicit tx: T): A

  def release(obj: Obj[T])(implicit tx: T): Unit

  def get[A](obj: Obj[T])(implicit tx: T): Option[A]

  implicit val universe: Universe[T]
}