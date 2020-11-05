package de.sciss.synth.proc

import java.io.File

import de.sciss.lucre.DataStore
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{WorkspacePlatformImpl => Impl}

trait WorkspacePlatform {
  def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace[_] /*[~] forSome { type ~ <: SSys[~] }*/ =
    Impl.read(dir, ds)  // IntelliJ highlight bug

  object Confluent {
    def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace.Confluent = Impl.readConfluent (dir, ds)
    def empty(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace.Confluent = Impl.emptyConfluent(dir, ds)
  }

  trait Confluent extends Workspace[proc.Confluent.Txn] {
    type S = proc.Confluent
    type T = proc.Confluent.Txn
    type D = proc.Durable  .Txn

    // have to restate this for some reason?
    // cf. http://stackoverflow.com/questions/16495522/pattern-matching-refuses-to-recognize-member-type-value-x-is-not-a-member-of-2
    // def system: S

    def cursors: Cursors[T, D]
  }

  object Durable {
    def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace.Durable = Impl.readDurable (dir, ds)
    def empty(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace.Durable = Impl.emptyDurable(dir, ds)
  }
  trait Durable extends Workspace[proc.Durable.Txn] {
    type S = proc.Durable
  }
}