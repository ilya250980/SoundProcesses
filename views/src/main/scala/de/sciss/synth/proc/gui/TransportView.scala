/*
 *  TransportView.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package gui

import impl.{TransportViewImpl => Impl}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.View
import de.sciss.lucre.stm
import de.sciss.audiowidgets.TimelineModel

object TransportView {
  /** Creates a new transport view.
    *
    * @param transport        the transport to control
    * @param timelineModel    the model whose position is used for play and updated after stop
    * @param hasMillis        if `true` display milliseconds
    * @param hasLoop          if `true` add a loop button
    * @param hasShortcuts     if `true` add keyboard accelerators
    */
  def apply[S <: Sys[S]](transport: Transport[S], timelineModel: TimelineModel,
                         hasMillis: Boolean = true, hasLoop: Boolean = true, hasShortcuts: Boolean = true)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): TransportView[S] =
    Impl[S](transport, timelineModel, hasMillis = hasMillis, hasLoop = hasLoop, hasShortcuts = hasShortcuts)
}

trait TransportView[S <: stm.Sys[S]] extends View[S] {
  def transport    : Transport[S]
  def timelineModel: TimelineModel
}