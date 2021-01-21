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
import de.sciss.lucre.{Cursor, DataStore, Source, confluent}
import de.sciss.proc
import de.sciss.proc.Workspace.MetaData
import de.sciss.proc.impl.WorkspaceImpl.{Data, Fmt}
import de.sciss.proc.{Cursors, Workspace}
import de.sciss.proc.{Confluent => Cf, Durable => Dur}
import de.sciss.serial.TFormat

import scala.collection.JavaConverters.asScalaSetConverter
import scala.util.Try

object WorkspacePlatformImpl {
  private implicit def ConfluentFmt : Fmt[Cf  .Txn] = WorkspaceImpl.fmt
  private implicit def DurableFmt   : Fmt[Dur .Txn] = WorkspaceImpl.fmt

  private def requireExists(dir: File): Unit =
    if (!(new File(dir, "open")).isFile) throw new FileNotFoundException(s"Workspace ${dir.getPath} does not exist")

  private def requireExistsNot(dir: File): Unit =
    if ((new File(dir, "open")).exists()) throw new IOException(s"Workspace ${dir.getPath} already exists")

  def read(dir: URI, ds: DataStore.Factory, meta: MetaData): Workspace[_] = {
    val dirF = new File(dir)
    requireExists(dirF)
    val fis = new FileInputStream(new File(dirF, "open"))
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
        val _res = readConfluent(dir, ds, meta) // IntelliJ highlight bug
        _res
      } else {
        val _res = readDurable(dir, ds, meta) // IntelliJ highlight bug
        _res
      }
    }
    res
  }

  def readConfluent(dir: URI, ds: DataStore.Factory, meta: MetaData): proc.Workspace.Confluent = {
    val dirF = new File(dir)
    requireExists(dirF)
    applyConfluent(dirF, ds = ds, meta = meta)
  }

  def emptyConfluent(dir: URI, ds: DataStore.Factory, meta: MetaData): proc.Workspace.Confluent = {
    val dirF = new File(dir)
    requireExistsNot(dirF)
    applyConfluent(dirF, ds = ds, meta = meta)
  }

  def readDurable(dir: URI, ds: DataStore.Factory, meta: MetaData): proc.Workspace.Durable = {
    val dirF = new File(dir)
    requireExists(dirF)
    applyDurable(dirF, ds = ds, meta = meta)
  }

  def emptyDurable(dir: URI, ds: DataStore.Factory, meta: MetaData): proc.Workspace.Durable = {
    val dirF = new File(dir)
    requireExistsNot(dirF)
    applyDurable(dirF, ds = ds, meta = meta)
  }

//  def buildInfVersion(pkg: String): Option[String] = buildInfString(pkg = pkg, key = "version")
//
//  def buildInfString(pkg: String, key: String): Option[String] = Try {
//    val clazz = Class.forName(s"$pkg.BuildInfo")
//    val m = clazz.getMethod(key)
//    m.invoke(null).toString
//  }.toOption

  private def openDataStore(dir: File, ds: DataStore.Factory, confluent: Boolean,
                            meta: MetaData): (DataStore.Factory, MetaData) = {
    val prop  = new Properties()
    val fOpen = new File(dir, "open")
    if (fOpen.isFile) { // read previous meta-data
      val fis = new FileInputStream(fOpen)
      try {
        prop.load(fis)
      } finally {
        fis.close()
      }
    }
    // XXX TODO if we introduce a `readOnly` mode, this should be the case
    val fos = new FileOutputStream(fOpen)
    try {
      prop.setProperty("type", if (confluent) "confluent" else "ephemeral")
//      // store version information for sp and known applications
//      buildInfVersion("de.sciss.proc"   ).foreach(prop.setProperty("soundprocesses-version" , _))
//      buildInfVersion("de.sciss.mellite").foreach(prop.setProperty("mellite-version"        , _))
//      buildInfVersion("at.iem.sysson"   ).foreach(prop.setProperty("sysson-version"         , _))
      meta.foreach { case (key, value) => prop.setProperty(key, value) }
      prop.store(fos, "Mellite Workspace Meta-Info")
    } finally {
      fos.close()
    }
    val metaNew = {
      val mb = Map.newBuilder[String, String]
      mb.sizeHint(prop.size)
      val keys = prop.stringPropertyNames().asScala
      keys.foreach { key =>
        val value = prop.getProperty(key)
        mb += ((key, value))
      }
      mb.result()
    }

    (ds, metaNew)
  }

  private def applyConfluent(dir: File, ds: DataStore.Factory, meta: MetaData): proc.Workspace.Confluent = {
    type S = Cf
    type T = Cf .Txn
    type D = Dur.Txn
    val (fact, metaNew) = openDataStore(dir, ds = ds, confluent = true, meta = meta)
    implicit val system: S = Cf(fact)
    implicit val fmt: TFormat[D, Cursors[T, D]] = Cursors.format  // help Dotty...
    val (access, cursors) = system.rootWithDurable[Data[T], Cursors[T, D]] { implicit tx: T =>
      Data[T]()
    } { implicit tx: D =>
      val c = Cursors[T, D](confluent.Access.root[T])
      c.name_=("root")
      c
    }

    new ConfluentImpl(dir, system, metaNew, access, cursors)
  }

  private def applyDurable(dir: File, ds: DataStore.Factory, meta: MetaData): proc.Workspace.Durable = {
    type S = Dur
    type T = Dur.Txn
    val (fact, metaNew) = openDataStore(dir, ds = ds, confluent = false, meta = meta)
    val system: S = Dur(fact)
    val access    = WorkspaceImpl.initAccess[T](system)
    new DurableImpl(dir, system, metaNew, access)
  }

  private def fileBase(f: File): String = {
    val n = f.getName
    val i = n.lastIndexOf('.')
    if (i < 0) n else n.substring(0, i)
  }

  private final class ConfluentImpl(_folder: File, val system: Cf, val meta: MetaData,
                                    protected val access: Source[Cf.Txn, Data[Cf.Txn]],
                                    val cursors: Cursors[Cf.Txn, Dur.Txn])
    extends proc.Workspace.Confluent with WorkspaceImpl[Cf.Txn] {

    def folder: Option[URI] = Some(_folder.toURI)

    def name: String = fileBase(_folder)

    val cursor: Cursor[Cf.Txn] =
      confluent.Cursor.wrap(cursors.cursor)(system)
  }

  private final class DurableImpl(_folder: File, val system: Dur, val meta: MetaData,
                                  protected val access: Source[Dur.Txn, Data[Dur.Txn]])
    extends proc.Workspace.Durable with WorkspaceImpl[Dur.Txn] {

    def folder: Option[URI] = Some(_folder.toURI)

    def name: String = fileBase(_folder)

    def cursor: Cursor[Dur.Txn] = system
  }
}
