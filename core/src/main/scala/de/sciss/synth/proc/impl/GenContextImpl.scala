/*
 *  GenContextImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Sys}

import scala.concurrent.stm.TMap

object GenContextImpl {
  private[this] val map = TMap.empty[WorkspaceHandle[_], GenContext[_]]

  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S],
                                  workspaceHandle: WorkspaceHandle[S]): GenContext[S] = {
    val res = map.get(workspaceHandle).getOrElse {
      val objMap  = tx.newInMemoryIDMap[ContextEntry[S]]
      val res0    = new Impl[S](objMap)
      map.put(workspaceHandle, res0)
      res0
    }
    res.asInstanceOf[GenContext[S]]
  }

  private final class Impl[S <: Sys[S]](protected val objMap: IdentifierMap[S#ID, S#Tx, ContextEntry[S]])
                                       (implicit val cursor: stm.Cursor[S], val workspaceHandle: WorkspaceHandle[S])
    extends ContextImpl[S] with GenContext[S] with Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = map.remove(workspaceHandle)
  }
}