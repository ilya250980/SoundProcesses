/*
 *  TransportView.scala
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

package de.sciss.proc.gui

import de.sciss.audiowidgets.{TimelineModel, TransportCatch}
import de.sciss.lucre.swing.View
import de.sciss.lucre.{Cursor, Txn, synth}
import de.sciss.proc.Transport
import de.sciss.proc.gui.impl.{TransportViewImpl => Impl}

object TransportView {
  /** Creates a new transport view.
    *
    * @param transport        the transport to control
    * @param timelineModel    the model whose position is used for play and updated after stop
    * @param hasMillis        if `true`, displays milliseconds
    * @param hasLoop          if `true`, adds a loop button
    * @param catchOption      if defined, adds a catch button
    * @param hasShortcuts     if `true`, adds keyboard accelerators
    */
  def apply[T <: synth.Txn[T]](transport: Transport[T], timelineModel: TimelineModel,
                               hasMillis    : Boolean = true,
                               hasLoop      : Boolean = true,
                               hasCatch     : Boolean = false,
                               hasShortcuts : Boolean = true)
                              (implicit tx: T, cursor: Cursor[T]): TransportView[T] =
    Impl[T](transport, timelineModel, hasMillis = hasMillis, hasLoop = hasLoop, hasCatch = hasCatch,
      hasShortcuts = hasShortcuts)
}

trait TransportView[T <: Txn[T]] extends View[T] {
  def transport    : Transport[T]
  def timelineModel: TimelineModel

  def isPlayingEDT: Boolean

  var catchOption: Option[TransportCatch]
}