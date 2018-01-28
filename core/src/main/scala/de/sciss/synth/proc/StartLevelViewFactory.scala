/*
 *  StartLevelViewFactory.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}

import scala.language.higherKinds

trait StartLevelViewFactory {
  def typeID: Int

  type Repr[~ <: Sys[~]] <: Obj[~]

  def mkStartLevelView[S <: SSys[S]](value: Repr[S])(implicit tx: S#Tx): ControlValuesView[S] // ScalarOptionView[S]
}