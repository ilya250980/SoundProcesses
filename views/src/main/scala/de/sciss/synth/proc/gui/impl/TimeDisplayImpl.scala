/*
 *  TimeDisplayImpl.scala
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

package de.sciss.synth.proc.gui.impl

import java.awt.{Color, Cursor}

import de.sciss.audiowidgets.impl.ActionGoToTime
import de.sciss.audiowidgets.{AxisFormat, LCDColors, LCDFont, LCDPanel, TimelineModel}
import de.sciss.desktop.impl.DynamicComponentImpl
import de.sciss.model.Change
import de.sciss.synth.proc.gui.TimeDisplay
import javax.swing.UIManager

import scala.swing.Swing._
import scala.swing.event.{MouseClicked, MouseEntered, MouseExited}
import scala.swing.{BoxPanel, Component, Label, Orientation}

final class TimeDisplayImpl(model: TimelineModel, hasMillis: Boolean)
  extends TimeDisplay {

  private[this] val lcdFormat = AxisFormat.Time(hours = true, millis = hasMillis)
  private[this] val lcd: Label = new Label with DynamicComponentImpl {
    // protected def component: Component = this

    private[this] val decimals  = if (hasMillis)  3 else 0
    private[this] val pad       = if (hasMillis) 12 else 8

    private[this] final val isDark  = UIManager.getBoolean("dark-skin")
    private[this] final val fgNorm  = if (isDark) LCDColors.blueFg else LCDColors.defaultFg
    private[this] final val fgHover = if (isDark) new Color(0x5E, 0x97, 0xFF) else Color.blue

    private def updateText(frame: Long): Unit = {
      val secs = frame / model.sampleRate
      text = lcdFormat.format(secs, decimals = decimals, pad = pad)
    }

    private val tlmListener: TimelineModel.Listener = {
      case TimelineModel.Position(_, Change(_, frame)) =>
        updateText(frame)
    }

    protected def componentShown(): Unit = {
      model.addListener(tlmListener)
      updateText(model.position)
    }

    protected def componentHidden(): Unit =
      model.removeListener(tlmListener)

    peer.putClientProperty("styleId", "noshade")
    font        = LCDFont() // .deriveFont(11.5f)
    foreground  = fgNorm
    updateText(model.position)

    maximumSize = preferredSize
    minimumSize = preferredSize

    model.modifiableOption.foreach { mod =>
      listenTo(mouse.clicks)
      listenTo(mouse.moves)
      reactions += {
        case MouseEntered(_, _, _) => foreground = fgHover
        case MouseExited (_, _, _) => foreground = fgNorm
        case MouseClicked(_, _, _, _, false) =>
          new ActionGoToTime(mod, null).apply()
      }
      cursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
    }
  }
  //      lcd.setMinimumSize(lcd.getPreferredSize)
  //      lcd.setMaximumSize(lcd.getPreferredSize)
  private val lcdFrame  = new LCDPanel {
    contents   += lcd
    maximumSize = preferredSize
    minimumSize = preferredSize
  }
  private val lcdPane = new BoxPanel(Orientation.Vertical) {
    contents += VGlue
    contents += lcdFrame
    contents += VGlue
  }

  def component: Component = lcdPane
}