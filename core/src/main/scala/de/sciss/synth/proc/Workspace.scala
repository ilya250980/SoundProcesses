/*
 *  Workspace.scala
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

import java.io.File

import de.sciss.lucre
import de.sciss.lucre.{DataStore, Workspace => LWorkspace}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{WorkspaceImpl => Impl}

object Workspace {
  /** File name extension (excluding leading period) */
  final val ext = "mllt"

  def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): LWorkspace[_] /*[~] forSome { type ~ <: SSys[~] }*/ =
    Impl.read(dir, ds)  // IntelliJ highlight bug

  object Confluent {
    def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Confluent = Impl.readConfluent (dir, ds)
    def empty(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Confluent = Impl.emptyConfluent(dir, ds)
  }

  trait Confluent extends LWorkspace[proc.Confluent.Txn] {
    type S = proc.Confluent
    type T = proc.Confluent.Txn

    // have to restate this for some reason?
    // cf. http://stackoverflow.com/questions/16495522/pattern-matching-refuses-to-recognize-member-type-value-x-is-not-a-member-of-2
    // def system: S

    def cursors: Cursors[T, S#D]
  }

  object Durable {
    def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Durable = Impl.readDurable (dir, ds)
    def empty(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Durable = Impl.emptyDurable(dir, ds)
  }
  trait Durable extends LWorkspace[proc.Durable.Txn] {
    type S = proc.Durable
  }

  object InMemory {
    def apply(): InMemory = Impl.applyInMemory()
  }
  trait InMemory extends LWorkspace[lucre.synth.InMemory.Txn] {
    type S = lucre.synth.InMemory
  }

  val Implicits: LWorkspace.Implicits.type = LWorkspace.Implicits
}