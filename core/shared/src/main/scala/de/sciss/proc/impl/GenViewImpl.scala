/*
 *  GenViewImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.{Obj, synth}
import de.sciss.proc.GenView.Factory
import de.sciss.proc.{GenView, Universe}

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

  def apply[T <: synth.Txn[T]](obj: Obj[T])(implicit tx: T, universe: Universe[T]): GenView[T] = {
    val tid = obj.tpe.typeId
    val opt: Option[Factory] = map.get(tid)
    val f = opt.getOrElse(sys.error(s"No GenView factory for type ${obj.tpe} / $tid"))
    f.apply[T](obj.asInstanceOf[f.Repr[T]])
  }

  private var map = scala.Predef.Map.empty[Int, Factory]
}
