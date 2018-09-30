/*
 *  UniverseView.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.View

trait UniverseView[S <: Sys[S]] extends View.Cursor[S] {
  implicit val universe: Universe[S]

  implicit def cursor: stm.Cursor[S] = universe.cursor
}
