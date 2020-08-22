/*
 *  FolderRunnerImpl.scala
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

import de.sciss.lucre.event.impl.DummyObservableImpl
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Runner.Attr
import de.sciss.synth.proc.{Runner, Universe}

import scala.concurrent.stm.Ref
import scala.util.control.NonFatal

object FolderRunnerImpl {
  def apply[S <: Sys[S]](obj: Folder[S])(implicit tx: S#Tx, universe: Universe[S]): Runner[S] = {
//    val rMap = tx.newInMemoryIdMap[Entry[S]]
    new Impl(/*tx.newHandle(obj),*/ /*rMap*/).init(obj)
  }

  // make sure we maintain the runners in the same order as the folder
  private final class Entry[S <: Sys[S]](val r: Runner[S]) {
    val pred: Ref[Entry[S]] = Ref.make()
    val succ: Ref[Entry[S]] = Ref.make()
  }

  private final class Impl[S <: Sys[S]](/*objH: stm.Source[S#Tx, Folder[S]],*/ /*rMap: IdentifierMap[S#Id, S#Tx, Entry[S]]*/)
                                       (implicit val universe: Universe[S])
    extends BasicRunnerImpl[S] {

    override def toString = s"Runner.Folder${hashCode().toHexString}"

    private[this] var obsF: Disposable[S#Tx] = _

    private[this] val rHead = Ref.make[Entry[S]]()

    object progress extends Runner.Progress[S#Tx] with DummyObservableImpl[S] {
      def current(implicit tx: S#Tx): Double = -1
    }

    private def mkEntry(child: Obj[S])(implicit tx: S#Tx): Entry[S] = {
      val rChild = try {
        Runner[S](child)
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          null    // we allow "un-runnable" children for now, so we don't get caught up with indices
      }
      new Entry(rChild)
    }

    def init(obj: Folder[S])(implicit tx: S#Tx): this.type = {
      var predSuccR: Ref[Entry[S]] = rHead
      var pred: Entry[S] = null
      obj.iterator.foreach { child =>
        val e = mkEntry(child)
        predSuccR() = e
        e.pred()    = pred
        pred        = e
        predSuccR   = e.succ
      }

      obsF = obj.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Folder.Added   (idx, child ) => addChild   (idx, child)
          case Folder.Removed (idx, _     ) => removeChild(idx /*, child*/)
        }
      }

      this
    }

    private def foreachChild(f: Runner[S] => Unit)(implicit tx: S#Tx): Unit = {
      var e = rHead()
      while (e != null) {
        if (e.r != null) f(e.r)
        e = e.succ()
      }
    }

    private def addChild(idx: Int, child: Obj[S])(implicit tx: S#Tx): Unit = {
      val e     = mkEntry(child)
      var succ  = rHead()
      var pred: Entry[S] = null
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
    }

    private def removeChild(idx: Int /*, child: Obj[S]*/)(implicit tx: S#Tx): Unit = {
      var e = rHead()
      var pred: Entry[S] = null
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

    def prepare(attr: Attr[S])(implicit tx: S#Tx): Unit = {
      foreachChild(_.prepare(attr)) // XXX TODO --- monitor and reflect `Preparing` like `AuralFolderImpl`
      state = Runner.Prepared
    }

    def run()(implicit tx: S#Tx): Unit = {
      foreachChild(_.run())
      state = Runner.Running
    }

    def stop()(implicit tx: S#Tx): Unit = {
      foreachChild(_.stop())
      state = Runner.Stopped
    }

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      obsF.dispose()
//      rMap.dispose()
      rHead() = null
    }
  }
}