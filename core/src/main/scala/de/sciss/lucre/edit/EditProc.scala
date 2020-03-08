/*
 *  EditProc.scala
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

import de.sciss.lucre.stm.{Folder, Sys}
import de.sciss.synth.proc.{Output, Proc}

object EditProc {
  def addLink[S <: Sys[S]](source: Output[S], sink: Proc[S], key: String = Proc.mainIn)
                          (implicit tx: S#Tx): Unit = {
    sink.attr.get(key) match {
      case Some(f: Folder[S]) =>
        EditFolder.append(parent = f, child = source)
      case Some(other) =>
        val f = Folder[S]()
        f.addLast(other)
        f.addLast(source)
        EditAttrMap.put(sink.attr, key = key, value = f)

      case None =>
        EditAttrMap.put(sink.attr, key = key, value = source)
    }
  }

  def hasLink[S <: Sys[S]](source: Output[S], sink: Proc[S], key: String = Proc.mainIn)
                           (implicit tx: S#Tx): Boolean = {
    sink.attr.get(key) match {
      case Some(`source`) => true

      case Some(f: Folder[S]) =>
        val idx = f.indexOf(source)
        idx >= 0

      case _ => false
    }
  }

  def removeLink[S <: Sys[S]](source: Output[S], sink: Proc[S], key: String = Proc.mainIn)
                             (implicit tx: S#Tx): Boolean = {
    val a = sink.attr
    a.get(key) match {
      case Some(`source`) =>
        EditAttrMap.remove(a, key)
        true

      case Some(f: Folder[S]) =>
        val idx = f.indexOf(source)
        idx >= 0 && {
          EditFolder.removeAt(f, idx)
          if (f.isEmpty) {
            EditAttrMap.remove(a, key)
          }
          true
        }

      case _ =>
        false
    }
  }
}
