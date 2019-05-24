/*
 *  EditFolder
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

package de.sciss.synth.proc.edit

import de.sciss.equal.Implicits._
import de.sciss.lucre.stm.UndoManager.CannotUndoException
import de.sciss.lucre.stm.impl.BasicUndoableEdit
import de.sciss.lucre.stm.{Folder, Obj, Sys, UndoManager}

object EditFolder {
  def append[S <: Sys[S]](parent: Folder[S], child: Obj[S])
                         (implicit tx: S#Tx): Unit =
    UndoManager.find[S].fold(
      appendDo  (parent, child)
    ) { implicit undo =>
      appendUndo(parent, child)
    }

  def appendUndo[S <: Sys[S]](parent: Folder[S], child: Obj[S])
                             (implicit tx: S#Tx, undo: UndoManager[S]): Unit = {
    appendDo(parent, child)
    val edit = new Append(parent, child, tx)
    undo.addEdit(edit)
  }

  private def appendDo[S <: Sys[S]](parent: Folder[S], child: Obj[S])(implicit tx: S#Tx): Unit =
    parent.addLast(child)

  private final class Append[S <: Sys[S]](parent0: Folder[S], child0: Obj[S], tx0: S#Tx)
    extends BasicUndoableEdit[S] {

    private[this] val parentH = tx0.newHandle(parent0)
    private[this] val childH  = tx0.newHandle(child0 )

    protected def undoImpl()(implicit tx: S#Tx): Unit = {
      val p = parentH()
      val c = childH ()
      val e = p.removeLast()
      if (e !== c) throw new CannotUndoException(s"$name: last element is not $c")
    }

    protected def redoImpl()(implicit tx: S#Tx): Unit =
      appendDo(parentH(), childH())

    def name: String = "Append to Folder"
  }
}
