/*
 *  GenViewImpl.scala
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
package impl

import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.synth.proc.GenView.Factory

object GenViewImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"GenView factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = {
    val tid = obj.tpe.typeID
    val opt: Option[Factory] = map.get(tid)
    val f = opt.getOrElse(sys.error(s"No GenView factory for type ${obj.tpe} / $tid"))
    f.apply[S](obj.asInstanceOf[f.Repr[S]])
  }

  private var map = scala.Predef.Map.empty[Int, Factory]
}
