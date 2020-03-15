/*
 *  StartLevelViewFactory.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}

trait StartLevelViewFactory {
  def tpe: Obj.Type

  type Repr[~ <: Sys[~]] <: Obj[~]

  def mkStartLevelView[S <: SSys[S]](value: Repr[S])(implicit tx: S#Tx): ControlValuesView[S] // ScalarOptionView[S]
}