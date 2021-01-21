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
import de.sciss.proc.Workspace.MetaData
import de.sciss.proc.impl.WorkspaceImpl
import de.sciss.{lucre, proc}

object Workspace extends WorkspacePlatform {
  /** File name extension (excluding leading period) */
  final val ext = "mllt"

  type MetaData = Map[String, String]
  
  /** Conventional key for meta-data field */
  final val KeySoundProcessesVersion  = "soundprocesses-version"
  /** Conventional key for meta-data field */
  final val KeyMelliteVersion         = "mellite-version"

  object InMemory {
    def apply(meta: MetaData = Map.empty): InMemory = WorkspaceImpl.applyInMemory(meta)
  }

  trait InMemory extends Workspace[lucre.synth.InMemory.Txn] {
    type S = lucre.synth.InMemory
  }

  object Blob {
//    def read(f: URI): Future[Workspace.Blob] = WorkspaceImpl.readBlob(f)

    def fromByteArray(xs: Array[Byte], meta: MetaData = Map.empty): Workspace.Blob =
      WorkspaceImpl.blobFromByteArray(xs, meta)

    def empty(meta: MetaData = Map.empty): Workspace.Blob = WorkspaceImpl.applyBlob(meta)
  }
  trait Blob extends Workspace[proc.Durable.Txn] {
    type S = proc.Durable

//    def write(f: URI): Future[Unit]

    def toByteArray(implicit tx: proc.Durable.Txn): Array[Byte]
  }

  /** Wraps an existing system into a workspace, assuming ephemeral (non-confluent) semantics.
    * This initialized the workspace, either detecting an existing root, or creating a new empty root.
    */
  def Ephemeral[T1 <: Txn[T1], S1 <: Sys { type T = T1 }](system: S1, meta: MetaData = Map.empty)
                                                         (implicit cursor: Cursor[T1]): proc.Workspace[T1] =
    WorkspaceImpl.applyEphemeral[T1, S1](system, meta)

  val Implicits: LWorkspace.Implicits.type = LWorkspace.Implicits
}
trait Workspace[T <: Txn[T]] extends LWorkspace[T] {
//  /** Since we obtain `Workspace[_]` from read methods, this is lesser evil, since we
//    * cannot make totally "wrong" casts here. */
//  def cast[T1 <: Txn[T1]]: Workspace[T1] = this.asInstanceOf[Workspace[T1]]

  def meta: MetaData
}