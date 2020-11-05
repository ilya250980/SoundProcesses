package de.sciss.synth.proc.impl

import java.net.URI

import de.sciss.lucre.synth.{InMemory => InMem}
import de.sciss.lucre.{Artifact, Cursor, Disposable, Folder, Source, Txn, TxnLike}
import de.sciss.serial.{DataInput, DataOutput, TFormat}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.WorkspaceImpl.Data
import de.sciss.synth.proc.{Workspace, log}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.{Ref, Txn => STMTxn}

object WorkspaceImpl {
  private[proc] final class Fmt[T <: Txn[T]] extends TFormat[T, Data[T]] {
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

  private implicit def InMemoryFmt : Fmt[InMem.Txn] = new Fmt[InMem.Txn]

  private final val COOKIE  = 0x4D656C6C69746500L  // "Mellite\0"
  private final val VERSION = 1

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

    new InMemoryWorkspaceImpl(system, access)
  }

  private[proc] abstract class Data[T <: Txn[T]] {
    def root: Folder[T]

    final def write(out: DataOutput): Unit = {
      out.writeLong(COOKIE | VERSION)
      root.write(out)
    }

    //    final def dispose()(implicit tx: T): Unit =
    //      root.dispose()

    override def toString = s"Data ($root)"
  }
}
trait WorkspaceImpl[T <: Txn[T]] {
  _: Workspace[T] =>

  // ---- abstract ----

  protected def access: Source[T, Data[T]]

  // ---- implemented ----

  override def toString = s"Workspace<${folder.fold("in-memory")(f => Artifact.Value.name(f))}>" // + hashCode().toHexString

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


final class InMemoryWorkspaceImpl(val system: InMem, protected val access: Source[InMem.Txn, Data[InMem.Txn]])
  extends proc.Workspace.InMemory with WorkspaceImpl[InMem.Txn] {

  // val systemType = implicitly[reflect.runtime.universe.TypeTag[InMem]]

  //    type I = system.I
  //    val inMemoryBridge: T => S#I#Tx  = tx => tx
  //    def inMemoryCursor: Cursor[I]   = system.inMemory

  def cursor: Cursor[InMem.Txn] = system

  def folder: Option[URI] = None
  def name = "in-memory"
}
