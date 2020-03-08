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

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{IdentifierMap, Sys, Workspace}
import de.sciss.synth.proc.GenContext

import scala.concurrent.stm.TMap

object GenContextImpl {
  private[this] val map = TMap.empty[Workspace[_], GenContext[_]]

  def apply[S <: Sys[S]]()(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         workspace: Workspace[S]): GenContext[S] = {
    val res = map.get(workspace).getOrElse {
      val objMap  = tx.newInMemoryIdMap[ContextEntry[S]]
      val res0    = new Impl[S](objMap)
      map.put(workspace, res0)
      res0
    }
    res.asInstanceOf[GenContext[S]]
  }

  private final class Impl[S <: Sys[S]](protected val objMap: IdentifierMap[S#Id, S#Tx, ContextEntry[S]])
                                       (implicit val cursor: stm.Cursor[S], val workspace: Workspace[S])
    extends ContextImpl[S] with GenContext[S] {

    def dispose()(implicit tx: S#Tx): Unit = map.remove(workspace)
  }
}