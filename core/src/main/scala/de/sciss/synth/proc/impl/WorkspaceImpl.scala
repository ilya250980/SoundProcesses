/*
 *  WorkspaceImpl.scala
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

package de.sciss.synth.proc.impl

import java.io.{File, FileInputStream, FileNotFoundException, FileOutputStream, IOException}
import java.util.Properties

import de.sciss.file._
import de.sciss.lucre.synth.{InMemory => InMem}
import de.sciss.lucre.{Cursor, DataStore, Disposable, Folder, Source, Txn, TxnLike, Workspace, confluent}
import de.sciss.serial.{DataInput, DataOutput, TFormat}
import de.sciss.synth.proc
import de.sciss.synth.proc.{Cursors, log, Confluent => Cf, Durable => Dur}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, Txn => STMTxn}
import scala.util.Try

object WorkspaceImpl {
  private final class Fmt[T <: Txn[T]] extends TFormat[T, Data[T]] {
    def write(data: Data[T], out: DataOutput): Unit =
      data.write(out)

    def readT(in: DataInput)(implicit tx: T): Data[T] = {
      val cookie = in.readLong()
      if ((cookie & 0xFFFFFFFFFFFFFF00L) != COOKIE) sys.error(s"Unexpected cookie $cookie (should be $COOKIE)")
      val version = cookie.toInt & 0xFF
      if (version != VERSION) sys.error(s"Incompatible file version (found $version, but need $VERSION)")

      new Data[T] {
        val root: Folder[T] = Folder.read(in)
      }
    }
  }

  private implicit val ConfluentFmt: Fmt[Cf.Txn]    = new Fmt[Cf.Txn]
  private implicit def DurableFmt  : Fmt[Dur.Txn]   = ConfluentFmt.asInstanceOf[Fmt[Dur.Txn]]
  private implicit def InMemoryFmt : Fmt[InMem.Txn] = ConfluentFmt.asInstanceOf[Fmt[InMem.Txn]]

  private def requireExists(dir: File): Unit =
    if (!(dir / "open").isFile) throw new FileNotFoundException(s"Workspace ${dir.path} does not exist")

  private def requireExistsNot(dir: File): Unit =
    if ((dir / "open").exists()) throw new IOException(s"Workspace ${dir.path} already exists")

  def read(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace[_] /*Workspace[~] forSome { type ~ <: SSys[~] }*/ = {
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
    val res: Workspace[_] /*~] forSome { type ~ <: synth.Txn[~] }*/ = {
      if (confluent) {
        val _res /*: Workspace[~] forSome { type ~ <: synth.Txn[~] }*/ = readConfluent(dir, ds) // IntelliJ highlight bug
        _res
      } else {
        val _res /*: Workspace[~] forSome { type ~ <: synth.Txn[~] }*/ = readDurable  (dir, ds) // IntelliJ highlight bug
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
    type T    = InMem.Txn
    implicit val system: S = InMem()

    val access = system.root[Data[T]] { implicit tx =>
      val data: Data[T] = new Data[T] {
        val root: Folder[T] = Folder()(tx)
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
    type T    = Cf.Txn
    val fact  = openDataStore(dir, ds = ds /* config = config */, confluent = true)
    implicit val system: S = Cf(fact)

    val (access, cursors) = system.rootWithDurable[Data[T], Cursors[T, S#D]] { implicit tx =>
      val data: Data[T] = new Data[T] {
        val root: Folder[T] = Folder()(tx)
      }
      data

    } { implicit tx =>
      val c   = Cursors[T, S#D](confluent.Access.root[T])
      c.name_=("root")
      c
    }

    new ConfluentImpl(dir, system, access, cursors)
  }

  private def applyDurable(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Durable = {
    type S    = Dur
    type T    = Dur.Txn
    val fact  = openDataStore(dir, ds = ds /* config = config */, confluent = false)
    implicit val system: S = Dur(fact)

    val access = system.root[Data[T]] { implicit tx =>
      val data: Data[T] = new Data[T] {
        val root: Folder[T] = Folder()(tx)
      }
      data
    }

    new DurableImpl(dir, system, access)
  }

  private final val COOKIE  = 0x4D656C6C69746500L  // "Mellite\0"
  private final val VERSION = 1

  private abstract class Data[T <: Txn[T]] {
    def root: Folder[T]

    final def write(out: DataOutput): Unit = {
      out.writeLong(COOKIE | VERSION)
      root.write(out)
    }

//    final def dispose()(implicit tx: T): Unit =
//      root.dispose()

    override def toString = s"Data ($root)"
  }

  private trait Impl[T <: Txn[T]] {
    _: Workspace[T] =>

    // ---- abstract ----

    protected def access: Source[T, Data[T]]

    // ---- implemented ----

    override def toString = s"Workspace<${folder.fold("in-memory")(_.name)}>" // + hashCode().toHexString

    private[this] val _dependents  = Ref(Vec.empty[Disposable[T]])

//    final val rootH: stm.Source[T, Folder[T]] = stm.Source.map(access)(_.root)

    final override def root(implicit tx: T): Folder[T] = access().root

    final override def addDependent   (dep: Disposable[T])(implicit tx: TxnLike): Unit =
      _dependents.transform(_ :+ dep)(tx.peer)

    final override def removeDependent(dep: Disposable[T])(implicit tx: TxnLike): Unit =
      _dependents.transform { in =>
        val idx = in.indexOf(dep)
        require(idx >= 0, s"Dependent $dep was not registered")
        in.patch(idx, Nil, 1)
      } (tx.peer)

    final override def dependents(implicit tx: TxnLike): Iterable[Disposable[T]] = _dependents.get(tx.peer)

    final override def close(): Unit = {
      // XXX TODO --- why did we have this asynchronously?
//      SoundProcesses.atomic[T, Unit] { implicit tx =>
//        dispose()
//      } (cursor)

      cursor.step { implicit tx => dispose() }
    }

    final def dispose()(implicit tx: T): Unit = {
      // logInfoTx(s"Dispose workspace $name")

      // first dispose all dependents
      val dep = _dependents.get(tx.peer)
      dep.foreach(_.dispose())
      _dependents.update(Vec.empty)(tx.peer)

      // if the transaction is successful...
      STMTxn.afterCommit { _ =>
        // ...and close the database
        log(s"Closing system $system")
        system.close()
      } (tx.peer)
    }
  }

  private final class ConfluentImpl(_folder: File, val system: Cf, protected val access: Source[Cf.Txn, Data[Cf.Txn]],
                                    val cursors: Cursors[Cf.Txn, Dur.Txn])
    extends proc.Workspace.Confluent with Impl[Cf.Txn] {

    def folder: Option[File]  = Some(_folder)
    def name  : String        = _folder.base

//    type I = system.I
//
//    val inMemoryBridge: T => S#I#Tx  = _.inMemory
//    def inMemoryCursor: Cursor[I]   = system.inMemory

    // def cursor = cursors.cursor
    val cursor: Cursor[T] = confluent.Cursor.wrap(cursors.cursor)(system)
  }

  private final class DurableImpl(_folder: File, val system: Dur,
                                  protected val access: Source[Dur.Txn, Data[Dur.Txn]])
    extends proc.Workspace.Durable with Impl[Dur.Txn] {

    def folder: Option[File]  = Some(_folder)
    def name  : String        = _folder.base

//    type I = system.I
//
//    val inMemoryBridge: T => S#I#Tx  = Dur.inMemory
//    def inMemoryCursor: Cursor[I]   = system.inMemory

    def cursor: Cursor[Dur.Txn] = system
  }

  private final class InMemoryImpl(val system: InMem, protected val access: Source[InMem.Txn, Data[InMem.Txn]])
    extends proc.Workspace.InMemory with Impl[InMem.Txn] {

    // val systemType = implicitly[reflect.runtime.universe.TypeTag[InMem]]

//    type I = system.I
//    val inMemoryBridge: T => S#I#Tx  = tx => tx
//    def inMemoryCursor: Cursor[I]   = system.inMemory

    def cursor: Cursor[InMem.Txn] = system

    def folder: Option[File] = None
    def name = "in-memory"
  }
}