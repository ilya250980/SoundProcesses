/*
 *  StartLevelViewFactory.scala
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

package de.sciss.proc

import de.sciss.lucre.{Obj, Txn, synth}

trait StartLevelViewFactory {
  def tpe: Obj.Type

  type Repr[~ <: Txn[~]] <: Obj[~]

  def mkStartLevelView[T <: synth.Txn[T]](value: Repr[T])(implicit tx: T): ControlValuesView[T] // ScalarOptionView[T]
}