/*
 *  GenContextImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{IdentifierMap, Sys}

object GenContextImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S],
                         workspaceHandle: WorkspaceHandle[S]): GenContext[S] = {
    val objMap  = tx.newInMemoryIDMap[ContextEntry[S]]
    val res     = new Impl[S](objMap)
    res
  }

  private final class Impl[S <: Sys[S]](protected val objMap: IdentifierMap[S#ID, S#Tx, ContextEntry[S]])
                                       (implicit val cursor: stm.Cursor[S], val workspaceHandle: WorkspaceHandle[S])
    extends ContextImpl[S] with GenContext[S]
}