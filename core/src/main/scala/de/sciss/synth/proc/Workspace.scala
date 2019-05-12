/*
 *  Workspace.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.io.File

import de.sciss.lucre
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{DataStore, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.serial.Serializer
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{WorkspaceImpl => Impl}

import scala.language.existentials

object Workspace {
  /** File name extension (excluding leading period) */
  final val ext = "mllt"

  type Group       [S <: Sys[S]] = BiGroup.Modifiable   [S, Proc[S] /* , Proc.Update[S] */]
  type GroupUpdate [S <: Sys[S]] = BiGroup.Update       [S, Proc[S] /* , Proc.Update[S] */]

  type Groups      [S <: Sys[S]] = stm.List.Modifiable[S, Group[S] /* , GroupUpdate[S] */]
  type GroupsUpdate[S <: Sys[S]] = stm.List.Update    [S, Group[S] /* , GroupUpdate[S] */]

  type Transports  [S <: SSys[S]] = stm.List.Modifiable[S, Transport[S] /* , Unit */] // Transport.Update[ S, Proc[ S ]]]

  def read (dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace[~] forSome { type ~ <: SSys[~] } =
    Impl.read(dir, ds)  // IntelliJ highlight bug

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

  val Implicits: stm.Workspace.Implicits.type = stm.Workspace.Implicits

//  object Implicits {
//    implicit def dummy[S <: SSys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Workspace[S] =
//      new DummyImpl[S](tx.system, cursor) // dummyVal.asInstanceOf[DummyImpl[S]]
//
////    private val dummyVal = new DummyImpl[NoSys]
//
//    private final class DummyImpl[S <: SSys[S]](val system: S, val cursor: stm.Cursor[S])
//      extends Workspace[S] {
//
//      type I = system.I
//      val inMemoryBridge: S#Tx => I#Tx  = system.inMemoryTx // _.inMemory
//      def inMemoryCursor: stm.Cursor[I] = system.inMemory
//
//      def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit = ()
//      def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit = ()
//
//      def dependents(implicit tx: TxnLike): Iterable[Disposable[S#Tx]] = Nil
//
//      def name: String = "dummy"
//
//      def root(implicit tx: S#Tx): Folder[S] =
//        throw new UnsupportedOperationException("No root folder on a dummy workspace handle")
//
//      def folder: Option[File] = None
//
//      def close(): Unit = ()
//
//      def dispose()(implicit tx: S#Tx): Unit = ()
//
////      def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A] = Vector.empty
//    }
//  }
}