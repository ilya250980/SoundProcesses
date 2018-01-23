/*
 *  AuralFolderLikeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.AuralObj.Container

import scala.concurrent.stm.Ref

trait AuralFolderLikeImpl[S <: Sys[S], Repr <: Obj[S], View <: AuralObj.FolderLike[S, View]]
  extends AuralObj.FolderLike[S, View] with ObservableImpl[S, AuralView.State] {
  impl: View =>

  // ---- abstract ----

  protected def transport: Transport[S]

  protected def performPlay(timeRef: TimeRef)(implicit tx: S#Tx): Unit

  protected def mkObserver(obj: Repr)(implicit tx: S#Tx): Disposable[S#Tx]

  // ---- impl ----

  private[this] var observer    : Disposable[S#Tx] = _
  private[this] var transportObs: Disposable[S#Tx] = _

  private[this] val currentStateRef = Ref[AuralView.State](AuralView.Stopped)

  final protected def processFolderUpdate(fUpd: expr.List.Update[S, Obj[S]])(implicit tx: S#Tx): Unit =
    fUpd.changes.foreach {
      case Folder.Added  (_, elem) => transport.addObject   (elem)
      case Folder.Removed(_, elem) => transport.removeObject(elem)
      case _ =>
    }

  final def init(obj: Repr)(implicit tx: S#Tx): this.type = {
    observer      = mkObserver(obj)
    transportObs  = transport.react { implicit tx => {
      case Transport.ViewAdded  (_, view) => contents(Container.ViewAdded  [S, View](impl, view.obj().id, view))
      case Transport.ViewRemoved(_, view) => contents(Container.ViewRemoved[S, View](impl, view.obj().id, view))
      case _ =>
    }}

    this
  }

  object contents extends ObservableImpl[S, Container.Update[S, View]] {
    def apply(update: Container.Update[S, View])(implicit tx: S#Tx): Unit = fire(update)
  }

  final def views(implicit tx: S#Tx): Set[AuralObj[S]] = transport.views

  final def getView    (obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]] = transport.getView    (obj)
  final def getViewById(id : S#ID  )(implicit tx: S#Tx): Option[AuralObj[S]] = transport.getViewById(id )

  final def stop()(implicit tx: S#Tx): Unit = {
    transport.stop()
    state = AuralView.Stopped
  }

  final def state(implicit tx: S#Tx): AuralView.State = currentStateRef.get(tx.peer)

  private def state_=(value: AuralView.State)(implicit tx: S#Tx): Unit = {
    val old = currentStateRef.swap(value)(tx.peer)
    if (value != old) {
      // println(s"------ENSEMBLE STATE $old > $value")
      fire(value)
    }
  }

  final protected def startTransport(offset: Long)(implicit tx: S#Tx): Unit = {
    transport.stop()
    transport.seek(offset)  // XXX TODO -- should we incorporate timeRef.frame) ?
    transport.play()        // XXX TODO -- should we be able to pass the timeRef?
  }

  final def play(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = {
    if (state == AuralView.Playing) return
    performPlay(timeRef.force)
    state = AuralView.Playing
  }

  final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
    if (state != AuralView.Stopped) return
    Console.err.println("TODO: AuralObj.FolderLike.prepare") // XXX TODO
    state = AuralView.Prepared
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    observer    .dispose()
    transportObs.dispose()
    transport   .dispose()
  }
}