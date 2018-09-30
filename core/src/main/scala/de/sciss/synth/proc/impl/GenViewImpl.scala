/*
 *  GenViewImpl.scala
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
package impl

import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.GenView.Factory

object GenViewImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeId
    if (map.contains(tid)) throw new IllegalArgumentException(s"GenView factory for type $tid already installed")
    map += tid -> f
  }

  def tryAddFactory(f: Factory): Boolean = sync.synchronized {
    val tid = f.typeId
    val res = !map.contains(tid)
    if (res) map += tid -> f
    res
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx, universe: Universe[S]): GenView[S] = {
    val tid = obj.tpe.typeId
    val opt: Option[Factory] = map.get(tid)
    val f = opt.getOrElse(sys.error(s"No GenView factory for type ${obj.tpe} / $tid"))
    f.apply[S](obj.asInstanceOf[f.Repr[S]])
  }

  private var map = scala.Predef.Map.empty[Int, Factory]
}
