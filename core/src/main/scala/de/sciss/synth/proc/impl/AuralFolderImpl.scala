/*
 *  AuralFolderImpl.scala
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

object AuralFolderImpl {
  def apply[S <: Sys[S]](folder: Folder[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Folder[S] = {
    val transport = Transport[S]
    folder.iterator.foreach(transport.addObject)
    new Impl(tx.newHandle(folder), transport).init(folder)
  }

  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Folder[S]],
                                        protected val transport: Transport[S])
    extends AuralFolderLikeImpl[S, Folder[S], AuralObj.Folder[S]]
    with AuralObj.Folder[S] { impl =>

    def typeID: Int = Folder.typeID

    def folder(implicit tx: S#Tx): Folder[S] = obj()

    def mkObserver(ens: Folder[S])(implicit tx: S#Tx): Disposable[S#Tx] =
      ens.changed.react { implicit tx => upd =>
        processFolderUpdate(upd)
      }

    protected def performPlay(timeRef: TimeRef)(implicit tx: S#Tx): Unit =
      startTransport(timeRef.offset)
  }
}
