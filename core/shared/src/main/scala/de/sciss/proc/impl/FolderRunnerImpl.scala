/*
 *  FolderRunnerImpl.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.Context
import de.sciss.lucre.impl.DummyObservableImpl
import de.sciss.lucre.{Disposable, Folder, Obj, Txn}
import de.sciss.proc.Runner.Attr
import de.sciss.proc.{Runner, Universe}

import scala.concurrent.stm.Ref

object FolderRunnerImpl {
  def apply[T <: Txn[T]](obj: Folder[T])(implicit tx: T, universe: Universe[T]): Runner[T] = {
//    val rMap = tx.newIdentMap[Entry[T]]
    // println("FolderRunnerImpl():")
    // (new Exception).printStackTrace()
    new Impl(/*tx.newHandle(obj),*/ /*rMap*/).init(obj)
  }

  // make sure we maintain the runners in the same order as the folder
  private final class Entry[T <: Txn[T]](val r: Runner[T]) {
    val pred: Ref[Entry[T]] = Ref.make()
    val succ: Ref[Entry[T]] = Ref.make()
  }

  private final class Impl[T <: Txn[T]](/*objH: stm.Source[T, Folder[T]],*/ /*rMap: IdentMap[Ident[T], T, Entry[T]]*/)
                                       (implicit val universe: Universe[T])
    extends BasicRunnerImpl[T] {

    override def toString = s"Runner.Folder${hashCode().toHexString}"

    private[this] val obsRef  = Ref.make[Disposable[T]]()
    private[this] val rHead   = Ref.make[Entry[T]]()
    private[this] val attrRef = Ref(Context.emptyAttr[T])

    object progress extends Runner.Progress[T] with DummyObservableImpl[T] {
      def current(implicit tx: T): Double = -1
    }

    private def mkEntry(child: Obj[T])(implicit tx: T): Entry[T] = {
      // we allow "un-runnable" children for now, so we don't get caught up with indices
      val rChild = Runner.get[T](child).orNull
      new Entry(rChild)
    }

    def init(obj: Folder[T])(implicit tx: T): this.type = {
      var predSuccR: Ref[Entry[T]] = rHead
      var pred: Entry[T] = null
      var CNT = 0
      obj.iterator.foreach { child =>
        val e = mkEntry(child)
        predSuccR() = e
        e.pred()    = pred
        pred        = e
        predSuccR   = e.succ
        CNT += 1
      }

      // println(s"FOLDER RUNNER INIT ${this.hashCode()}")
      obsRef() = obj.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Folder.Added   (idx, child ) => addChild   (idx, child)
          case Folder.Removed (idx, _     ) => removeChild(idx /*, child*/)
        }
      }

      this
    }

    private def foreachChild(f: Runner[T] => Unit)(implicit tx: T): Unit = {
      var e = rHead()
      while (e != null) {
        if (e.r != null) f(e.r)
        e = e.succ()
      }
    }

    private def addChild(idx: Int, child: Obj[T])(implicit tx: T): Unit = {
      // println(s"FOLDER RUNNER ADD $idx")
      val e     = mkEntry(child)
      var succ  = rHead()
      var pred: Entry[T] = null
      var rem = idx
      while (rem > 0) {
        pred = succ
        succ = succ.succ()
        rem -= 1
      }
      if (succ != null) succ.pred() = e
      val predSuccR = if (pred != null) pred.succ else rHead
      predSuccR() = e

      e.pred() = pred
      e.succ() = succ

      state match {
        case Runner.Preparing | Runner.Prepared =>
          e.r.prepare(attrRef())
        case Runner.Running =>
          e.r.prepare(attrRef())
          e.r.run()
        case _ => // Stopped, Failed
      }
    }

    private def removeChild(idx: Int /*, child: Obj[T]*/)(implicit tx: T): Unit = {
      // println(s"FOLDER RUNNER REMOVE $idx")
      var e = rHead()
      var pred: Entry[T] = null
      var rem = idx
      while (rem > 0) {
        pred = e
        e    = e.succ()
        rem -= 1
      }
      val succ = e.succ()
      if (succ != null) succ.pred() = pred
      val predSuccR = if (pred != null) pred.succ else rHead
      predSuccR() = succ
      if (e.r != null) e.r.dispose()
    }

    def prepare(attr: Attr[T])(implicit tx: T): Unit = {
      attrRef() = attr
      foreachChild(_.prepare(attr)) // XXX TODO --- monitor and reflect `Preparing` like `AuralFolderImpl`
      state = Runner.Prepared
    }

    def run()(implicit tx: T): Unit = {
      foreachChild(_.run())
      state = Runner.Running
    }

    def stop()(implicit tx: T): Unit = {
      foreachChild(_.stop())
      attrRef() = Context.emptyAttr[T]
      state = Runner.Stopped
    }

    protected def disposeData()(implicit tx: T): Unit = {
      // println(s"FOLDER RUNNER DISPOSE ${this.hashCode()}")
      // Note: it's crucial not to call dispose twice on the
      // observer (the event reaction map will complain).
      // Because of Runner.Mutable, it's possible that
      // `disposeData` is called twice.
      obsRef.swap(Disposable.empty[T]).dispose()
      attrRef() = Context.emptyAttr[T]
      foreachChild(_.dispose())
//      rMap.dispose()
      rHead() = null
    }
  }
}