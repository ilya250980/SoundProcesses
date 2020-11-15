/*
 *  Ops.scala
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

package de.sciss.proc

import de.sciss.lucre.{Txn, expr}

import scala.language.implicitConversions

object Ops extends Ops
trait Ops extends expr.Ops /*with OpsPlatform*/ {
  implicit def audioCueObjOps[T <: Txn[T]](obj: AudioCue.Obj[T]): AudioCue.Obj.Ops[T] = new AudioCue.Obj.Ops(obj)
}