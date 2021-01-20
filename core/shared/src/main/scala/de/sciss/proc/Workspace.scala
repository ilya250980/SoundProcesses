/*
 *  Workspace.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.{Cursor, Sys, Txn, Workspace => LWorkspace}
import de.sciss.proc.impl.WorkspaceImpl
import de.sciss.{lucre, proc}

object Workspace extends WorkspacePlatform {
  /** File name extension (excluding leading period) */
  final val ext = "mllt"

  object InMemory {
    def apply(): InMemory = WorkspaceImpl.applyInMemory()
  }

  trait InMemory extends Workspace[lucre.synth.InMemory.Txn] {
    type S = lucre.synth.InMemory
  }

  /** Wraps an existing system into a workspace, assuming ephemeral (non-confluent) semantics.
    * This initialized the workspace, either detecting an existing root, or creating a new empty root.
    */
  def Ephemeral[T1 <: Txn[T1], S1 <: Sys { type T = T1 }](system: S1)
                                                         (implicit cursor: Cursor[T1]): proc.Workspace[T1] =
    WorkspaceImpl.applyEphemeral[T1, S1](system)

  val Implicits: LWorkspace.Implicits.type = LWorkspace.Implicits
}
trait Workspace[T <: Txn[T]] extends LWorkspace[T] {
//  /** Since we obtain `Workspace[_]` from read methods, this is lesser evil, since we
//    * cannot make totally "wrong" casts here. */
//  def cast[T1 <: Txn[T1]]: Workspace[T1] = this.asInstanceOf[Workspace[T1]]
}