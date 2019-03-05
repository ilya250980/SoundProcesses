/*
 *  ControlValuesView.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.ugen.ControlValues

trait ControlValuesView[S <: Sys[S]]
  extends Observable[S#Tx, Option[ControlValues]] with stm.Source[S#Tx, Option[ControlValues]]
