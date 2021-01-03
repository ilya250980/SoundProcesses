/*
 *  TimeDisplay.scala
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

package de.sciss.proc.gui

import de.sciss.audiowidgets.TimelineModel
import de.sciss.proc.gui.impl.{TimeDisplayImpl => Impl}

import scala.swing.Component

object TimeDisplay {
  def apply(model: TimelineModel, hasMillis: Boolean): TimeDisplay =
    new Impl(model, hasMillis = hasMillis)
}
trait TimeDisplay {
  def component: Component
}