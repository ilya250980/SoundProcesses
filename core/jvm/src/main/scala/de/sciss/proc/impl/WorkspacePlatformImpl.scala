/*
 *  WorkspacePlatformImpl.scala
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

package de.sciss.proc.impl

import java.io.{File, FileInputStream, FileNotFoundException, FileOutputStream, IOException}
import java.net.URI
import java.util.Properties

import de.sciss.lucre.{Cursor, DataStore, Folder, Source, confluent}
import de.sciss.proc
import de.sciss.proc.impl.WorkspaceImpl.{Data, Fmt}
import de.sciss.proc.{Cursors, Workspace}
import de.sciss.proc.{Confluent => Cf, Durable => Dur}
import de.sciss.serial.TFormat

import scala.util.Try

object WorkspacePlatformImpl {
  private implicit val ConfluentFmt: Fmt[Cf.Txn] = new Fmt[Cf.Txn]

  private implicit def DurableFmt: Fmt[Dur.Txn] = ConfluentFmt.asInstanceOf[Fmt[Dur.Txn]]

  private def requireExists(dir: File): Unit =
    if (!(new File(dir, "open")).isFile) throw new FileNotFoundException(s"Workspace ${dir.getPath} does not exist")

  private def requireExistsNot(dir: File): Unit =
    if ((new File(dir, "open")).exists()) throw new IOException(s"Workspace ${dir.getPath} already exists")

  def read(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): Workspace[_] /*Workspace[~] forSome { type ~ <: SSys[~] }*/ = {
    requireExists(dir)
    val fis = new FileInputStream(new File(dir, "open"))
    val prop = new Properties
    prop.load(fis)
    fis.close()
    val confluent = prop.getProperty("type") match {
      case "confluent" => true
      case "ephemeral" => false
      case other => sys.error(s"Invalid property 'type': $other")
    }
    val res: Workspace[_] /*~] forSome { type ~ <: synth.Txn[~] }*/ = {
      if (confluent) {
        val _res /*: Workspace[~] forSome { type ~ <: synth.Txn[~] }*/ = readConfluent(dir, ds) // IntelliJ highlight bug
        _res
      } else {
        val _res /*: Workspace[~] forSome { type ~ <: synth.Txn[~] }*/ = readDurable(dir, ds) // IntelliJ highlight bug
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

  private def buildInfVersion(pkg: String): Option[String] = buildInfString(pkg = pkg, key = "version")

  private def buildInfString(pkg: String, key: String): Option[String] = Try {
    val clazz = Class.forName(s"$pkg.BuildInfo")
    val m = clazz.getMethod(key)
    m.invoke(null).toString
  }.toOption

  private def openDataStore(dir: File, ds: DataStore.Factory, confluent: Boolean): DataStore.Factory = {
    val fos = new FileOutputStream(new File(dir, "open"))
    try {
      val prop = new Properties()
      prop.setProperty("type", if (confluent) "confluent" else "ephemeral")
      // store version information for sp and known applications
      buildInfVersion("de.sciss.proc").foreach(prop.setProperty("soundprocesses-version", _))
      buildInfVersion("de.sciss.mellite").foreach(prop.setProperty("mellite-version", _))
      buildInfVersion("at.iem.sysson").foreach(prop.setProperty("sysson-version", _))
      prop.store(fos, "Mellite Workspace Meta-Info")
    } finally {
      fos.close()
    }
    ds
  }

  private def applyConfluent(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Confluent = {
    type S = Cf
    type T = Cf .Txn
    type D = Dur.Txn
    val fact = openDataStore(dir, ds = ds /* config = config */ , confluent = true)
    implicit val system: S = Cf(fact)
    implicit val fmt: TFormat[D, Cursors[T, D]] = Cursors.format  // help Dotty...
    val (access, cursors) = system.rootWithDurable[Data[T], Cursors[T, D]] { implicit tx: T =>
      val data: Data[T] = new Data[T] {
        val root: Folder[T] = Folder[T]()(tx)
      }
      data

    } { implicit tx: D =>
      val c = Cursors[T, D](confluent.Access.root[T])
      c.name_=("root")
      c
    }

    new ConfluentImpl(dir, system, access, cursors)
  }

  private def applyDurable(dir: File, ds: DataStore.Factory /* config: BerkeleyDB.Config */): proc.Workspace.Durable = {
    type S = Dur
    type T = Dur.Txn
    val fact = openDataStore(dir, ds = ds /* config = config */ , confluent = false)
    implicit val system: S = Dur(fact)

    val access = system.root[Data[T]] { implicit tx =>
      val data: Data[T] = new Data[T] {
        val root: Folder[T] = Folder()(tx)
      }
      data
    }

    new DurableImpl(dir, system, access)
  }

  private def fileBase(f: File): String = {
    val n = f.getName
    val i = n.lastIndexOf('.')
    if (i < 0) n else n.substring(0, i)
  }

  private final class ConfluentImpl(_folder: File, val system: Cf, protected val access: Source[Cf.Txn, Data[Cf.Txn]],
                                    val cursors: Cursors[Cf.Txn, Dur.Txn])
    extends proc.Workspace.Confluent with WorkspaceImpl[Cf.Txn] {

    def folder: Option[URI] = Some(_folder.toURI)

    def name: String = fileBase(_folder) // .base

    //    type I = system.I
    //
    //    val inMemoryBridge: T => S#I#Tx  = _.inMemory
    //    def inMemoryCursor: Cursor[I]   = system.inMemory

    // def cursor = cursors.cursor
    val cursor: Cursor[T] = confluent.Cursor.wrap(cursors.cursor)(system)
  }

  private final class DurableImpl(_folder: File, val system: Dur,
                                  protected val access: Source[Dur.Txn, Data[Dur.Txn]])
    extends proc.Workspace.Durable with WorkspaceImpl[Dur.Txn] {

    def folder: Option[URI] = Some(_folder.toURI)

    def name: String = fileBase(_folder) // .base

    //    type I = system.I
    //
    //    val inMemoryBridge: T => S#I#Tx  = Dur.inMemory
    //    def inMemoryCursor: Cursor[I]   = system.inMemory

    def cursor: Cursor[Dur.Txn] = system
  }

}
