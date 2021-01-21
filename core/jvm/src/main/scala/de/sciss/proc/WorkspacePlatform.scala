/*
 *  WorkspacePlatform.scala
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

import de.sciss.lucre.DataStore
import de.sciss.proc
import de.sciss.proc.Workspace.MetaData
import de.sciss.proc.impl.{WorkspacePlatformImpl => Impl}

import java.net.URI

trait WorkspacePlatform {
  def read(dir: URI, ds: DataStore.Factory, meta: MetaData = Map.empty): Workspace[_] =
    Impl.read(dir, ds, meta)

  object Confluent {
    def read (dir: URI, ds: DataStore.Factory, meta: MetaData = Map.empty): Workspace.Confluent =
      Impl.readConfluent (dir, ds, meta)

    def empty(dir: URI, ds: DataStore.Factory, meta: MetaData = Map.empty): Workspace.Confluent =
      Impl.emptyConfluent(dir, ds, meta)
  }

  trait Confluent extends Workspace[proc.Confluent.Txn] {
    type S = proc.Confluent
    type T = proc.Confluent.Txn
    type D = proc.Durable.Txn

    // have to restate this for some reason?
    // cf. http://stackoverflow.com/questions/16495522/pattern-matching-refuses-to-recognize-member-type-value-x-is-not-a-member-of-2
    // def system: S

    def cursors: Cursors[T, D]
  }

  object Durable {
    def read (dir: URI, ds: DataStore.Factory, meta: MetaData = Map.empty): Workspace.Durable =
      Impl.readDurable(dir, ds, meta)

    def empty(dir: URI, ds: DataStore.Factory, meta: MetaData = Map.empty): Workspace.Durable =
      Impl.emptyDurable(dir, ds, meta)
  }

  trait Durable extends Workspace[proc.Durable.Txn] {
    type S = proc.Durable
  }
}
