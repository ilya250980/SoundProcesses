/*
 *  EditObj.scala
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

package de.sciss.lucre.edit

import de.sciss.equal.Implicits._
import de.sciss.lucre.{Copy, Folder, Obj, Txn}
import de.sciss.proc.Proc

object EditObj {
  /** Makes a copy of an object, including all attributes.
    * If `connect` is false, does not put `Proc.mainIn`.
    *
    * Does not produce any undoable edits.
    *
    * @param in  the process to copy
    */
  def copyDo[T <: Txn[T]](in: Obj[T], connectInput: Boolean)(implicit tx: T): Obj[T] = {
    val context = Copy[T, T]()
    val out     = context.copyPlain(in)
    val attrIn  = in .attr
    val attrOut = out.attr
    attrIn.iterator.foreach { case (key, valueIn) =>
      if (key !== Proc.mainIn) {
        val valueOut = context(valueIn)
        attrOut.put(key, valueOut)
      } else if (connectInput) {
        val valueOpt = attrIn.get(Proc.mainIn).collect {
          case op: Proc.Output[T] => op
          case fIn: Folder[T] =>
            val fOut = Folder[T]()
            fIn.iterator.foreach { op => fOut.addLast(op) }
            fOut
        }
        valueOpt.foreach(value => attrOut.put(Proc.mainIn, value))
      }
    }
    context.finish()
    out
  }
}
