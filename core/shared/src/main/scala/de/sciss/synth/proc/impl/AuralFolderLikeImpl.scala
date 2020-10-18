/*
 *  AuralFolderLikeImpl.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.{Disposable, Folder, Ident, Obj, Txn}
import de.sciss.synth.proc.AuralObj.Container
import de.sciss.synth.proc.{AuralObj, Runner, TimeRef, Transport}

import scala.concurrent.stm.TMap

trait AuralFolderLikeImpl[T <: Txn[T], View <: AuralObj.FolderLike[T, View]]
  extends AuralObj.FolderLike[T, View] with BasicAuralObjImpl[T] {
  impl: View =>

  // ---- abstract ----

  protected def transport: Transport[T]

  protected def performPlay(timeRef: TimeRef)(implicit tx: T): Unit

  protected def mkObserver(obj: Repr)(implicit tx: T): Disposable[T]

  // ---- impl ----

  private[this] var observer    : Disposable[T] = _
  private[this] var transportObs: Disposable[T] = _

  private[this] val refPrepare = TMap.empty[AuralObj[T], Disposable[T]]

  final protected def processFolderUpdate(fUpd: Folder.Update[T])(implicit tx: T): Unit =
    fUpd.changes.foreach {
      case Folder.Added  (_, elem) => transport.addObject   (elem)
      case Folder.Removed(_, elem) => transport.removeObject(elem)
      case _ =>
    }

  final def init(obj: Repr)(implicit tx: T): this.type = {
    observer      = mkObserver(obj)
    transportObs  = transport.react { implicit tx => { // XXX TODO should probably check if `Preparing` or `Running`
      case Transport.ViewAdded  (_, view) => contents(Container.ViewAdded  [T, View](impl, view.obj.id, view))
      case Transport.ViewRemoved(_, view) => contents(Container.ViewRemoved[T, View](impl, view.obj.id, view))
      case _ =>
    }}

    this
  }

  object contents extends ObservableImpl[T, Container.Update[T, View]] {
    def apply(update: Container.Update[T, View])(implicit tx: T): Unit = fire(update)
  }

  final def views(implicit tx: T): Set[AuralObj[T]] = transport.views

  final def getView    (obj: Obj  [T])(implicit tx: T): Option[AuralObj[T]] = transport.getView    (obj)
  final def getViewById(id : Ident[T])(implicit tx: T): Option[AuralObj[T]] = transport.getViewById(id )

  final def stop()(implicit tx: T): Unit = {
    disposePrepare()
    transport.stop()
    state = Runner.Stopped
  }

  final protected def startTransport(offset: Long)(implicit tx: T): Unit = {
    transport.stop()
    transport.seek(offset)  // XXX TODO -- should we incorporate timeRef.frame) ?
    transport.play()        // XXX TODO -- should we be able to pass the timeRef?
  }

  final def run(timeRef: TimeRef.Option, unit: Unit)(implicit tx: T): Unit = {
    if (state == Runner.Running) return
    disposePrepare()
    performPlay(timeRef.force)
    state = Runner.Running
  }

  final def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = {
    if (state != Runner.Stopped) return
    views.foreach { v =>
      v.prepare(timeRef)
      if (v.state == Runner.Preparing) {
        val vObs = v.react { implicit tx =>vStateNow =>
          if (vStateNow != Runner.Preparing) { // XXX TODO --- we should probably reflect Failed, Done, Stopped
            refPrepare.remove(v).foreach { vObs0 =>
              vObs0.dispose()
              if (refPrepare.isEmpty && state == Runner.Preparing) {
                state = Runner.Prepared
              }
            }
          }
        }
        refPrepare.put(v, vObs)
      }
    }
    state = Runner.Prepared
  }

  private def disposePrepare()(implicit tx: T): Unit =
    if (!refPrepare.isEmpty) {
      refPrepare.foreach(_._2.dispose())
      refPrepare.clear()
    }

  def dispose()(implicit tx: T): Unit = {
    disposePrepare()
    observer    .dispose()
    transportObs.dispose()
    transport   .dispose()
  }
}