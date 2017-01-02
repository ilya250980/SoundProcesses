/*
 *  AuralEnsembleImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.model.Change

object AuralEnsembleImpl {
  def apply[S <: Sys[S]](obj: Ensemble[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] = {
    val transport = Transport[S]
    val ensemble  = obj
    ensemble.folder.iterator.foreach(transport.addObject)
    new Impl(tx.newHandle(obj), transport).init(ensemble)
  }
  
  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Ensemble[S]],
                                        protected val transport: Transport[S])
    extends AuralFolderLikeImpl[S, Ensemble[S], AuralObj.Ensemble[S]]
    with AuralObj.Ensemble[S] { impl =>
    
    def typeID: Int = Ensemble.typeID

    def folder(implicit tx: S#Tx): Folder[S] = ensemble.folder

    def mkObserver(ens: Ensemble[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      ens.changed.react { implicit tx => upd =>
        val ens = upd.ensemble
        upd.changes.foreach {
          case Ensemble.Folder (fUpd) => processFolderUpdate(fUpd)
          // case Ensemble.Offset (Change(_, newOffset )) =>
          case Ensemble.Playing(Change(_, newPlaying)) =>
            logTransport(s"AuralEnsemble - new playing.value = $newPlaying")
            if (newPlaying) {
              if (state == AuralView.Playing) startTransport(ens.offset.value)
            } else {
              transport.stop()
            }
          case _ =>
        }
      }

    private def ensemble(implicit tx: S#Tx): Ensemble[S] = obj()

    protected def performPlay(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val ens = ensemble
      val p   = ens.playing.value
      logTransport(s"AuralEnsemble.play() - playing.value = $p")
      if (p) startTransport(ens.offset.value)
    }
  }
}
