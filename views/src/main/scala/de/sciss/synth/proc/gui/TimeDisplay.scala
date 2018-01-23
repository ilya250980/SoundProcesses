/*
 *  TimeDisplay.scala
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

import scala.swing.Component
import de.sciss.audiowidgets.TimelineModel
import impl.{TimeDisplayImpl => Impl}

object TimeDisplay {
  def apply(model: TimelineModel, hasMillis: Boolean): TimeDisplay = new Impl(model, hasMillis = hasMillis)
}
trait TimeDisplay {
  def component: Component
}