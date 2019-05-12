/*
 *  WorkspaceImpl.scala
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

package de.sciss.synth.proc.impl

import java.io.{File, FileInputStream, FileNotFoundException, FileOutputStream, IOException}
import java.util.Properties

import de.sciss.file._
import de.sciss.lucre.stm.{DataStore, Disposable, Folder, Sys, TxnLike, Workspace}
import de.sciss.lucre.synth.{InMemory => InMem, Sys => SSys}
import de.sciss.lucre.{confluent, stm}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Cursors, log, Confluent => Cf, Durable => Dur}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, Txn}
import scala.language.existentials
import scala.util.Try

object WorkspaceImpl {
  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Data[S]] {
    def write(data: Data[S], out: DataOutput): Unit =
      data.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data[S] = {
      val cookie = in.readLong()
      if ((cookie & 0xFFFFFFFFFFFFFF00L) != COOKIE) sys.error(s"Unexpected cookie $cookie (should be $COOKIE)")
      val version = cookie.toInt & 0xFF
      if (version != VERSION) sys.error(s"Incompatible file version (found $version, but need $VERSION)")

      new Data[S] {
        val root: Folder[S] = Folder.read(in, access)
      }
    }
  }

  private implicit val ConfluentSer: Ser[Cf]    = new Ser[Cf]
  private implicit def DurableSer  : Ser[Dur]   = ConfluentSer.asInstanceOf[Ser[Dur]]
  private implicit def InMemorySer : Ser[InMem] = ConfluentSer.asInstanceOf[Ser[InMem]]

  private def requireExists(dir: File): Unit =
    if (!(dir / "open").isFile) throw new FileNotFoundException(s"Workspace ${dir.path} does not exist")

  private def requireExistsNot(dir: File): Unit =
    if ((dir / "open").exists()) throw new IOException(s"Workspace ${dir.path} already exists")

  def read(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace[~] forSome { type ~ <: SSys[~] } = {
    requireExists(dir)
    val fis   = new FileInputStream(dir / "open")
    val prop  = new Properties
    prop.load(fis)
    fis.close()
    val confluent = prop.getProperty("type") match {
      case "confluent"  => true
      case "ephemeral"  => false
      case other        => sys.error(s"Invalid property 'type': $other")
    }
    val res: Workspace[~] forSome { type ~ <: SSys[~] } = {
      if (confluent) {
        val _res: Workspace[~] forSome { type ~ <: SSys[~] } = readConfluent(dir, ds)
        _res
      } else {
        val _res: Workspace[~] forSome { type ~ <: SSys[~] } = readDurable(dir, ds)
        _res
      }
    }
    res // .asInstanceOf[Workspace[~ forSome { type ~ <: SSys[~] }]]
  }

//  private def setAllowCreate(in: BerkeleyDB.Config, value: Boolean): BerkeleyDB.Config =
//    if (in.allowCreate == value) in else {
//      val b = BerkeleyDB.ConfigBuilder(in)
//      b.allowCreate = value
//      b.build
//    }

  def readConfluent(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Confluent = {
    requireExists(dir)
    applyConfluent(dir, ds = ds /* config = setAllowCreate(config, value = false) */)
  }

  def emptyConfluent(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Confluent = {
    requireExistsNot(dir)
    applyConfluent(dir, ds = ds /* config = setAllowCreate(config, value = true) */)
  }

  def readDurable(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Durable = {
    requireExists(dir)
    applyDurable(dir, ds = ds /* config = setAllowCreate(config, value = false) */)
  }

  def emptyDurable(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Durable = {
    requireExistsNot(dir)
    applyDurable(dir, ds = ds /* config = setAllowCreate(config, value = true) */)
  }

  def applyInMemory(): proc.Workspace.InMemory = {
    type S    = InMem
    implicit val system: S = InMem()

    val access = system.root[Data[S]] { implicit tx =>
      val data: Data[S] = new Data[S] {
        val root: Folder[S] = Folder(tx)
      }
      data
    }

    new InMemoryImpl(system, access)
  }

  private def buildInfVersion(pkg: String): Option[String] = buildInfString(pkg = pkg, key = "version")

  private def buildInfString(pkg: String, key: String): Option[String] = Try {
    val clazz = Class.forName(s"$pkg.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } .toOption

  private def openDataStore(dir: File, ds: DataStore.Factory, confluent: Boolean): DataStore.Factory = {
    val fos = new FileOutputStream(dir / "open")
    try {
      val prop = new Properties()
      prop.setProperty("type", if (confluent) "confluent" else "ephemeral")
      // store version information for sp and known applications
      buildInfVersion("de.sciss.synth.proc").foreach(prop.setProperty("soundprocesses-version", _))
      buildInfVersion("de.sciss.mellite"   ).foreach(prop.setProperty("mellite-version"       , _))
      buildInfVersion("at.iem.sysson"      ).foreach(prop.setProperty("sysson-version"        , _))
      prop.store(fos, "Mellite Workspace Meta-Info")
    } finally {
      fos.close()
    }
    ds
  }

  private def applyConfluent(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Confluent = {
    type S    = Cf
    val fact  = openDataStore(dir, ds = ds /* config = config */, confluent = true)
    implicit val system: S = Cf(fact)

    val (access, cursors) = system.rootWithDurable[Data[S], Cursors[S, S#D]] { implicit tx =>
      val data: Data[S] = new Data[S] {
        val root: Folder[S] = Folder(tx)
      }
      data

    } { implicit tx =>
      val c   = Cursors[S, S#D](confluent.Access.root[S])
      c.name_=("master")
      c
    }

    new ConfluentImpl(dir, system, access, cursors)
  }

  private def applyDurable(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Durable = {
    type S    = Dur
    val fact  = openDataStore(dir, ds = ds /* config = config */, confluent = false)
    implicit val system: S = Dur(fact)

    val access = system.root[Data[S]] { implicit tx =>
      val data: Data[S] = new Data[S] {
        val root: Folder[S] = Folder(tx)
      }
      data
    }

    new DurableImpl(dir, system, access)
  }

  private final val COOKIE  = 0x4D656C6C69746500L  // "Mellite\0"
  private final val VERSION = 1

  private abstract class Data[S <: Sys[S]] {
    def root: Folder[S]

    final def write(out: DataOutput): Unit = {
      out.writeLong(COOKIE | VERSION)
      root.write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit =
      root.dispose()

    override def toString = s"Data ($root)"
  }

  private trait Impl[S <: Sys[S]] {
    _: Workspace[S] =>

    // ---- abstract ----

    protected def access: stm.Source[S#Tx, Data[S]]

    // ---- implemented ----

    override def toString = s"Workspace<${folder.fold("in-memory")(_.name)}>" // + hashCode().toHexString

    private[this] val _dependents  = Ref(Vec.empty[Disposable[S#Tx]])

    final val rootH: stm.Source[S#Tx, Folder[S]] = stm.Source.map(access)(_.root)

    final def root(implicit tx: S#Tx): Folder[S] = access().root

    final def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit =
      _dependents.transform(_ :+ dep)(tx.peer)

    final def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit =
      _dependents.transform { in =>
        val idx = in.indexOf(dep)
        require(idx >= 0, s"Dependent $dep was not registered")
        in.patch(idx, Nil, 1)
      } (tx.peer)

    final def dependents(implicit tx: TxnLike): Iterable[Disposable[S#Tx]] = _dependents.get(tx.peer)

//    final def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A] = {
//      val b   = Vector.newBuilder[A]
//      val fun = pf.lift
//
//      def loop(f: Folder[S]): Unit =
//        f.iterator.foreach { obj =>
//          fun(obj).foreach(b += _)
//          obj match {
//            case ef: Folder[S] => loop(ef)
//            case _ =>
//          }
//        }
//
//      loop(root)
//      b.result()
//    }

//    final def close(): Unit = {
//      atomic[S, Unit] { implicit tx =>
//        dispose()
//      } (cursor)
//    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      // logInfoTx(s"Dispose workspace $name")

      // first dispose all dependents
      val dep = _dependents.get(tx.peer)
      dep.foreach(_.dispose())
      _dependents.update(Vec.empty)(tx.peer)

      // if the transaction is successful...
      Txn.afterCommit { _ =>
        // ...and close the database
        log(s"Closing system $system")
        system.close()
      } (tx.peer)
    }
  }

  private final class ConfluentImpl(_folder: File, val system: Cf, protected val access: stm.Source[Cf#Tx, Data[Cf]],
                                    val cursors: Cursors[Cf, Cf#D])
    extends proc.Workspace.Confluent with Impl[Cf] {

    def folder: Option[File]  = Some(_folder)
    def name  : String        = _folder.base

    type I = system.I
    val inMemoryBridge: S#Tx => S#I#Tx  = _.inMemory
    def inMemoryCursor: stm.Cursor[I]   = system.inMemory

    // def cursor = cursors.cursor
    val cursor: stm.Cursor[S] = confluent.Cursor.wrap(cursors.cursor)(system)
  }

  private final class DurableImpl(_folder: File, val system: Dur,
                                  protected val access: stm.Source[Dur#Tx, Data[Dur]])
    extends proc.Workspace.Durable with Impl[Dur] {

    def folder: Option[File]  = Some(_folder)
    def name  : String        = _folder.base

    type I = system.I
    val inMemoryBridge: S#Tx => S#I#Tx  = Dur.inMemory
    def inMemoryCursor: stm.Cursor[I]   = system.inMemory

    def cursor: stm.Cursor[S] = system
  }

  private final class InMemoryImpl(val system: InMem, protected val access: stm.Source[InMem#Tx, Data[InMem]])
    extends proc.Workspace.InMemory with Impl[InMem] {

    // val systemType = implicitly[reflect.runtime.universe.TypeTag[InMem]]

    type I = system.I
    val inMemoryBridge: S#Tx => S#I#Tx  = tx => tx
    def inMemoryCursor: stm.Cursor[I]   = system.inMemory

    def cursor: stm.Cursor[S] = system

    def folder: Option[File] = None
    def name = "in-memory"
  }
}