/*
 *  Ops.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.expr
import de.sciss.lucre.stm.Sys

import scala.language.implicitConversions

object Ops extends Ops
trait Ops extends expr.Ops {
  implicit def audioCueObjOps[S <: Sys[S]](obj: AudioCue.Obj[S]): AudioCue.Obj.Ops[S] = new AudioCue.Obj.Ops(obj)
}
