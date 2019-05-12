/*
 *  AuralContextImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.synth.proc.{AuralContext, Universe, logAural}

object AuralContextImpl {
  def apply[S <: Sys[S]](server: Server)
                        (implicit tx: S#Tx, universe: Universe[S]): AuralContext[S] = {
    val objMap  = tx.newInMemoryIdMap[ContextEntry[S]]
    val auxMap  = tx.newInMemoryIdMap[Any]
//    import scheduler.cursor
//    val gen     = GenContext[S]
    val res     = new Impl[S](objMap, auxMap, server, tx)
    logAural(s"create context ${res.hashCode().toHexString}")
    // (new Throwable).printStackTrace()
    res
  }

  private final class Impl[S <: Sys[S]](protected val objMap: IdentifierMap[S#Id, S#Tx, ContextEntry[S]],
                                        protected val auxMap: IdentifierMap[S#Id, S#Tx, Any],
                                        val server          : Server,
                                        tx0: S#Tx)
                                       (implicit val universe: Universe[S])
    extends ContextImpl[S] with AuralContext[S] with AuxContextImpl[S] {

//    implicit val scheduler  : Scheduler       [S] = handler.scheduler
//    implicit def workspace  : WorkspaceHandle [S] = handler.workspace
//    implicit def genContext : GenContext      [S] = handler.genContext

    protected val auxObservers: IdentifierMap[S#Id, S#Tx, List[AuxObserver]] = tx0.newInMemoryIdMap
  }
}