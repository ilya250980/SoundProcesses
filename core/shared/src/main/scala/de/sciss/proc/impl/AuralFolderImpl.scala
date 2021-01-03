/*
 *  AuralFolderImpl.scala
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

import de.sciss.lucre.{Disposable, Folder, Obj, Source, Txn, synth}
import de.sciss.proc.{AuralContext, AuralObj, Runner, TimeRef, Transport}

object AuralFolderImpl {
  def apply[T <: synth.Txn[T]](folder: Folder[T], attr: Runner.Attr[T])
                              (implicit tx: T, context: AuralContext[T]): AuralObj.Folder[T] = {
    val transport = Transport[T](context, attr)
    folder.iterator.foreach(transport.addObject)  // XXX TODO: should we pass `attr`?
    new Impl(tx.newHandle(folder), transport).init(folder)
  }

  private final class Impl[T <: Txn[T]](objH: Source[T, Folder[T]],
                                        protected val transport: Transport[T])
    extends AuralFolderLikeImpl[T, /*Folder[T],*/ AuralObj.Folder[T]]
    with AuralObj.Folder[T] { impl =>

    def tpe: Obj.Type = Folder

    type Repr = Folder[T]

    def obj   (implicit tx: T): Folder[T] = objH()
    def folder(implicit tx: T): Folder[T] = objH()

    def mkObserver(ens: Folder[T])(implicit tx: T): Disposable[T] =
      ens.changed.react { implicit tx => upd =>
        processFolderUpdate(upd)
      }

    protected def performPlay(timeRef: TimeRef)(implicit tx: T): Unit =
      startTransport(timeRef.offset)
  }
}
