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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.Container
import de.sciss.synth.proc.{AuralObj, Runner, TimeRef, Transport}

import scala.concurrent.stm.TMap

trait AuralFolderLikeImpl[S <: Sys[S], /*Repr <: Obj[S],*/ View <: AuralObj.FolderLike[S, View]]
  extends AuralObj.FolderLike[S, View] with BasicAuralObjImpl[S] {
  impl: View =>

  // ---- abstract ----

  protected def transport: Transport[S]

  protected def performPlay(timeRef: TimeRef)(implicit tx: S#Tx): Unit

  protected def mkObserver(obj: Repr)(implicit tx: S#Tx): Disposable[S#Tx]

  // ---- impl ----

  private[this] var observer    : Disposable[S#Tx] = _
  private[this] var transportObs: Disposable[S#Tx] = _

  private[this] val refPrepare = TMap.empty[AuralObj[S], Disposable[S#Tx]]

  final protected def processFolderUpdate(fUpd: stm.Folder.Update[S])(implicit tx: S#Tx): Unit =
    fUpd.changes.foreach {
      case Folder.Added  (_, elem) => transport.addObject   (elem)
      case Folder.Removed(_, elem) => transport.removeObject(elem)
      case _ =>
    }

  final def init(obj: Repr)(implicit tx: S#Tx): this.type = {
    observer      = mkObserver(obj)
    transportObs  = transport.react { implicit tx => { // XXX TODO should probably check if `Preparing` or `Running`
      case Transport.ViewAdded  (_, view) => contents(Container.ViewAdded  [S, View](impl, view.obj.id, view))
      case Transport.ViewRemoved(_, view) => contents(Container.ViewRemoved[S, View](impl, view.obj.id, view))
      case _ =>
    }}

    this
  }

  object contents extends ObservableImpl[S, Container.Update[S, View]] {
    def apply(update: Container.Update[S, View])(implicit tx: S#Tx): Unit = fire(update)
  }

  final def views(implicit tx: S#Tx): Set[AuralObj[S]] = transport.views

  final def getView    (obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]] = transport.getView    (obj)
  final def getViewById(id : S#Id  )(implicit tx: S#Tx): Option[AuralObj[S]] = transport.getViewById(id )

  final def stop()(implicit tx: S#Tx): Unit = {
    disposePrepare()
    transport.stop()
    state = Runner.Stopped
  }

  final protected def startTransport(offset: Long)(implicit tx: S#Tx): Unit = {
    transport.stop()
    transport.seek(offset)  // XXX TODO -- should we incorporate timeRef.frame) ?
    transport.play()        // XXX TODO -- should we be able to pass the timeRef?
  }

  final def run(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = {
    if (state == Runner.Running) return
    disposePrepare()
    performPlay(timeRef.force)
    state = Runner.Running
  }

  final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
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

  private def disposePrepare()(implicit tx: S#Tx): Unit =
    if (!refPrepare.isEmpty) {
      refPrepare.foreach(_._2.dispose())
      refPrepare.clear()
    }

  def dispose()(implicit tx: S#Tx): Unit = {
    disposePrepare()
    observer    .dispose()
    transportObs.dispose()
    transport   .dispose()
  }
}