/*
 *  AuralContextImpl.scala
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

import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.synth.{Server, Sys}

object AuralContextImpl {
  def apply[S <: Sys[S]](server: Server, sched: Scheduler[S])
                        (implicit tx: S#Tx, workspaceHandle: WorkspaceHandle[S]): AuralContext[S] = {
    val objMap  = tx.newInMemoryIdMap[ContextEntry[S]]
    val auxMap  = tx.newInMemoryIdMap[Any]
    import sched.cursor
    val gen     = GenContext[S]
    val res     = new Impl[S](objMap, auxMap, sched, server, gen, tx)
    logAural(s"create context ${res.hashCode().toHexString}")
    // (new Throwable).printStackTrace()
    res
  }

  private final class Impl[S <: Sys[S]](protected val objMap: IdentifierMap[S#Id, S#Tx, ContextEntry[S]],
                                        protected val auxMap: IdentifierMap[S#Id, S#Tx, Any],
                                        val scheduler       : Scheduler[S],
                                        val server          : Server,
                                        val gen             : GenContext[S],
                                        tx0: S#Tx)
                                       (implicit val workspaceHandle: WorkspaceHandle[S])
    extends ContextImpl[S] with AuralContext[S] with AuxContextImpl[S] {

    protected val auxObservers: IdentifierMap[S#Id, S#Tx, List[AuxObserver]] = tx0.newInMemoryIdMap
  }
}