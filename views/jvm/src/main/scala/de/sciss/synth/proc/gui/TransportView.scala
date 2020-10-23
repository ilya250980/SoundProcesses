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

package de.sciss.synth.proc.gui

import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.swing.View
import de.sciss.lucre.{Cursor, Txn, synth}
import de.sciss.synth.proc.Transport
import de.sciss.synth.proc.gui.impl.{TransportViewImpl => Impl}

object TransportView {
  /** Creates a new transport view.
    *
    * @param transport        the transport to control
    * @param timelineModel    the model whose position is used for play and updated after stop
    * @param hasMillis        if `true` display milliseconds
    * @param hasLoop          if `true` add a loop button
    * @param hasShortcuts     if `true` add keyboard accelerators
    */
  def apply[T <: synth.Txn[T]](transport: Transport[T], timelineModel: TimelineModel,
                         hasMillis: Boolean = true, hasLoop: Boolean = true, hasShortcuts: Boolean = true)
                        (implicit tx: T, cursor: Cursor[T]): TransportView[T] =
    Impl[T](transport, timelineModel, hasMillis = hasMillis, hasLoop = hasLoop, hasShortcuts = hasShortcuts)
}

trait TransportView[T <: Txn[T]] extends View[T] {
  def transport    : Transport[T]
  def timelineModel: TimelineModel
}