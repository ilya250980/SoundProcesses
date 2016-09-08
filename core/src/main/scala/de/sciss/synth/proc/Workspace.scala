/*
 *  Workspace.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.io.File

import de.sciss.lucre
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.stm.{DataStore, Disposable, Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.{expr, stm}
import de.sciss.synth.proc.impl.{WorkspaceImpl => Impl}
import de.sciss.serial.Serializer
import de.sciss.synth.proc

import scala.collection.immutable.{IndexedSeq => Vec}

object Workspace {
  /** File name extension (excluding leading period) */
  final val ext = "mllt"

  type Group       [S <: Sys[S]] = BiGroup.Modifiable   [S, Proc[S] /* , Proc.Update[S] */]
  type GroupUpdate [S <: Sys[S]] = BiGroup.Update       [S, Proc[S] /* , Proc.Update[S] */]

  type Groups      [S <: Sys[S]] = expr.List.Modifiable[S, Group[S] /* , GroupUpdate[S] */]
  type GroupsUpdate[S <: Sys[S]] = expr.List.Update    [S, Group[S] /* , GroupUpdate[S] */]

  type Transports  [S <: SSys[S]] = expr.List.Modifiable[S, Transport[S] /* , Unit */] // Transport.Update[ S, Proc[ S ]]]

  def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): WorkspaceLike = Impl.read(dir, ds)

  object Confluent {
    def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Confluent = Impl.readConfluent (dir, ds)
    def empty(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Confluent = Impl.emptyConfluent(dir, ds)
  }

  trait Confluent extends Workspace[proc.Confluent] {
    type S = proc.Confluent

    // have to restate this for some reason?
    // cf. http://stackoverflow.com/questions/16495522/pattern-matching-refuses-to-recognize-member-type-value-x-is-not-a-member-of-2
    // def system: S

    def cursors: Cursors[S, S#D]
    }

  object Durable {
    def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Durable = Impl.readDurable (dir, ds)
    def empty(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Durable = Impl.emptyDurable(dir, ds)
  }
  trait Durable extends Workspace[proc.Durable] {
    type S = proc.Durable
  }

  object InMemory {
    def apply(): InMemory = Impl.applyInMemory()
  }
  trait InMemory extends Workspace[lucre.synth.InMemory] {
    type S = lucre.synth.InMemory
  }

  object Serializers {
    implicit def group[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Group[S]] =
      BiGroup.Modifiable.serializer[S, Proc[S] /* , Proc.Update[S] */ ] // (_.changed)
  }
}
sealed trait WorkspaceLike {
  def folder: Option[File]
  def name: String

  /** Issues a transaction that closes and disposes the workspace. */
  def close(): Unit
}
sealed trait Workspace[S <: Sys[S]] extends WorkspaceLike with WorkspaceHandle[S] with Disposable[S#Tx] {
//  import de.sciss.mellite.Workspace.{Group => _}

  // type System = S

  implicit def system: S

  def cursor: stm.Cursor[S]
  
  type I <: SSys[I]
  implicit def inMemoryBridge: S#Tx => I#Tx
  implicit def inMemoryCursor: stm.Cursor[I]

  def rootH: stm.Source[S#Tx, Folder[S]]

  def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A]

  // implicit def systemType: reflect.runtime.universe.TypeTag[S]
}