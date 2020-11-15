/*
 *  GenContextImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.{Cursor, IdentMap, Txn, Workspace}
import de.sciss.proc.GenContext

import scala.concurrent.stm.TMap

object GenContextImpl {
  private[this] val map = TMap.empty[Workspace[_], GenContext[_]]

  def apply[T <: Txn[T]]()(implicit tx: T, cursor: Cursor[T],
                         workspace: Workspace[T]): GenContext[T] = {
    val res = map.get(workspace).getOrElse {
      val objMap  = tx.newIdentMap[ContextEntry[T]]
      val res0    = new Impl[T](objMap)
      map.put(workspace, res0)
      res0
    }
    res.asInstanceOf[GenContext[T]]
  }

  private final class Impl[T <: Txn[T]](protected val objMap: IdentMap[T, ContextEntry[T]])
                                       (implicit val cursor: Cursor[T], val workspace: Workspace[T])
    extends ContextImpl[T] with GenContext[T] {

    def dispose()(implicit tx: T): Unit = {
      map.remove(workspace)
      ()
    }
  }
}