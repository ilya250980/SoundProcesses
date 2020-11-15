/*
 *  AuralContextImpl.scala
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

import de.sciss.lucre.synth.Server
import de.sciss.lucre.{IdentMap, Txn}
import de.sciss.synth.proc.{AuralContext, Universe}
import de.sciss.synth.proc.SoundProcesses.logAural

object AuralContextImpl {
  def apply[T <: Txn[T]](server: Server)
                        (implicit tx: T, universe: Universe[T]): AuralContext[T] = {
    val objMap  = tx.newIdentMap[ContextEntry[T]]
    val auxMap  = tx.newIdentMap[Any]
//    import scheduler.cursor
//    val gen     = GenContext[T]
    val res     = new Impl[T](objMap, auxMap, server, tx)
    logAural.debug(s"create context ${res.hashCode().toHexString}")
    // (new Throwable).printStackTrace()
    res
  }

  private final class Impl[T <: Txn[T]](protected val objMap: IdentMap[T, ContextEntry[T]],
                                        protected val auxMap: IdentMap[T, Any],
                                        val server          : Server,
                                        tx0: T)
                                       (implicit val universe: Universe[T])
    extends ContextImpl[T] with AuralContext[T] with AuxContextImpl[T] {

//    implicit val scheduler  : Scheduler       [T] = handler.scheduler
//    implicit def workspace  : WorkspaceHandle [T] = handler.workspace
//    implicit def genContext : GenContext      [T] = handler.genContext

    protected val auxObservers: IdentMap[T, List[AuxObserver]] = tx0.newIdentMap
  }
}