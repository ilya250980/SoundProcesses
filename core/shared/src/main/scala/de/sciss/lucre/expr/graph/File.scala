/*
 *  File.scala
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

package de.sciss.lucre.expr.graph

import java.io.IOException
import java.net.{URI => _URI}
import de.sciss.asyncfile.{AsyncFile, AsyncFileSystem}
import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, ExElem, IAction}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.{Cursor, IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.lucre.synth.Executor
import de.sciss.model.Change
import de.sciss.proc.SoundProcesses

import scala.concurrent.Future
import scala.concurrent.stm.Ref

object File extends FilePlatform {
  private def getFileSystem(uri: _URI): Future[AsyncFileSystem] = {
    val scheme = Option(uri.getScheme).getOrElse("file")
    val p = AsyncFile.getFileSystemProvider(scheme).getOrElse(
      throw new IOException(s"No file system for scheme $scheme")
    )
    import Executor.executionContext
    p.obtain()
  }

  object TmpDir extends ProductReader[TmpDir] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): TmpDir = {
      require (arity == 0 && adj == 0)
      new TmpDir()
    }
  }
  final case class TmpDir() extends Ex[_URI] {
    override def productPrefix: String = s"File$$TmpDir"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, _URI]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new Const.Expanded[T, _URI](tmpDir)
  }

  private final class MkDirExpanded[T <: Txn[T]](f: IExpr[T, _URI])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val uri = f.value
      tx.afterCommit {
        val fsFut = getFileSystem(uri)
        // XXX TODO how to handle error?
        // plural! we want to ensure parent directories are created, too
        import Executor.executionContext
        fsFut.foreach { fs =>
          fs.mkDirs(uri)
        }
      }
    }
  }

  object MkDir extends ProductReader[MkDir] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): MkDir = {
      require (arity == 1 && adj == 0)
      val _f = in.readEx[_URI]()
      new MkDir(_f)
    }
  }
  final case class MkDir(f: Ex[_URI]) extends Act {
    override def productPrefix: String = s"File$$MkDir"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new MkDirExpanded(f.expand[T])
  }

  private final class DeleteExpanded[T <: Txn[T]](f: IExpr[T, _URI])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      val uri = f.value
      tx.afterCommit {
        val fsFut = getFileSystem(uri)
        import Executor.executionContext
        // XXX TODO how to handle error?
        fsFut.foreach { fs =>
          fs.delete(uri)
        }
      }
    }
  }

  object Delete extends ProductReader[Delete] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Delete = {
      require (arity == 1 && adj == 0)
      val _f = in.readEx[_URI]()
      new Delete(_f)
    }
  }
  final case class Delete(f: Ex[_URI]) extends Act {
    override def productPrefix: String = s"File$$Delete"  // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new DeleteExpanded(f.expand[T])
  }

  private final class ListExpanded[T <: Txn[T]](dir: IExpr[T, _URI])(implicit protected val targets: ITargets[T],
                                                                     cursor: Cursor[T])
    extends IExpr[T, Seq[_URI]] with IActionImpl[T] with IChangeGeneratorEvent[T, Seq[_URI]] {

    private[this] val ref = Ref(Seq.empty[_URI])

    def value(implicit tx: T): Seq[_URI] = ref()

    override def changed: IChangeEvent[T, Seq[_URI]] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Seq[_URI] =
      pull.resolveExpr(this)

    def executeAction()(implicit tx: T): Unit = {
      val dv      = dir.value
      tx.afterCommit {
        val fsFut   = getFileSystem(dv)
        import Executor.executionContext
        fsFut.foreach { fs =>
          val nowFut = fs.listDir(dv)
          nowFut.foreach { now =>
            SoundProcesses.step[T]("File.List().executeAction()") { implicit tx =>
              val before  = ref.swap(now)
              if (before !== now) {
                fire(Change(before, now))
              }
            }
          }
        }
      }
    }
  }

  // XXX TODO --- this is synchronous design and cannot work on .js
  object List extends ProductReader[List] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): List = {
      require (arity == 1 && adj == 0)
      val _dir = in.readEx[_URI]()
      new List(_dir)
    }
  }
  final case class List(dir: Ex[_URI]) extends Ex[Seq[_URI]] with Act {
    override def productPrefix: String = s"File$$List"  // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Seq[_URI]] with IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.{cursor, targets}
      new ListExpanded(dir.expand[T])
    }
  }
}
