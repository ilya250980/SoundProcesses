/*
 *  Gen.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Publisher
import de.sciss.lucre.stm.{Obj, Sys}

object Gen {
  trait Update[S <: Sys[S]] {
    def gen  : Gen[S]
    def value: Option[Obj[S]]
  }

  // sealed trait Value[S <: Sys[S]]
}
trait Gen[S <: Sys[S]] extends Obj[S] with Publisher[S, Gen.Update[S]] {
  def valueType: Obj.Type
  def value(implicit tx: S#Tx): Option[Obj[S]]
}
