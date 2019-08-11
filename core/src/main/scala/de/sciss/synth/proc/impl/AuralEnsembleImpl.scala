/*
 *  AuralEnsembleImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Folder, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.model.Change
import de.sciss.synth.proc.{AuralContext, AuralObj, Ensemble, Runner, TimeRef, Transport, logTransport}

object AuralEnsembleImpl {
  def apply[S <: Sys[S]](obj: Ensemble[S], attr: Runner.Attr[S])
                        (implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Ensemble[S] = {
    val transport = Transport[S](context)
    val ensemble  = obj
    ensemble.folder.iterator.foreach(transport.addObject) // XXX TODO --- should we pass `attr`?
    new Impl(tx.newHandle(obj), transport).init(ensemble)
  }
  
  private final class Impl[S <: Sys[S]](objH: stm.Source[S#Tx, Ensemble[S]],
                                        protected val transport: Transport[S])
    extends AuralFolderLikeImpl[S, /*Ensemble[S],*/ AuralObj.Ensemble[S]]
    with AuralObj.Ensemble[S] { impl =>

    def tpe: Obj.Type = Ensemble

    def folder(implicit tx: S#Tx): Folder[S] = ensemble.folder

    override def obj(implicit tx: S#Tx): Ensemble[S] = objH()

    def mkObserver(ens: Ensemble[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      ens.changed.react { implicit tx => upd =>
        val ens = upd.ensemble
        upd.changes.foreach {
          case Ensemble.Folder (fUpd) => processFolderUpdate(fUpd)
          // case Ensemble.Offset (Change(_, newOffset )) =>
          case Ensemble.Playing(Change(_, newPlaying)) =>
            logTransport(s"AuralEnsemble - new playing.value = $newPlaying")
            if (newPlaying) {
              if (state === Runner.Running) startTransport(ens.offset.value)
            } else {
              transport.stop()
            }
          case _ =>
        }
      }

    private def ensemble(implicit tx: S#Tx): Ensemble[S] = objH()

    protected def performPlay(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val ens = ensemble
      val p   = ens.playing.value
      logTransport(s"AuralEnsemble.play() - playing.value = $p")
      if (p) startTransport(ens.offset.value)
    }
  }
}
