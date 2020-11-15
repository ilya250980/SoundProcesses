/*
 *  AuralOutputImpl.scala
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

import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.synth.{AudioBus, NodeRef}
import de.sciss.lucre.{Ident, Source, Txn}
import de.sciss.proc.{AuralContext, AuralObj, AuralOutput, Proc}
import de.sciss.proc.SoundProcesses.{logAural => logA}

object AuralOutputImpl {
  def apply[T <: Txn[T]](view: AuralObj.Proc[T], output: Proc.Output[T], bus: AudioBus)
                        (implicit tx: T, context: AuralContext[T]): AuralOutput.Owned[T] = {
    val id    = output.id
    val key   = output.key
    val res   = new Impl[T](view = view, key = key, bus = bus, idH = tx.newHandle(id))
    logA.debug(s"AuralOutput($view, $key, bus = $bus)")
    context.putAux[AuralOutput /* .Proxy */[T]](id, res)
    res
  }

  // ----------------------------------

  // note: it is crucial that we use `stm.Source[T, Ident[T]]` instead of just `Ident[T]`, because if
  // the view is created in the same transaction as the scan, the id's path will be empty, causing
  // an error in `dispose()` when trying to remove the entry from the id map!
  private final class Impl[T <: Txn[T]](val view: AuralObj.Proc[T], val key: String, val bus: AudioBus,
                                        idH: Source[T, Ident[T]])
    extends AuralOutput.Owned[T] with ObservableImpl[T, AuralOutput.Update] {

    override def toString: String = s"AuralOutput($bus)"

    def play(n: NodeRef)(implicit tx: T): Unit = {
//      implicit val itx = tx.peer
      logA.debug(s"AuralOutput play; $view, $key")
      fire(AuralOutput.Play(n))
    }

    def stop()(implicit tx: T): Unit = {
      logA.debug(s"AuralOutput stop; $view, $key")
      fire(AuralOutput.Stop)
    }

    def dispose()(implicit tx: T): Unit = {
      logA.debug(s"AuralOutput dispose; $view, $key")
//      implicit val itx = tx.peer
      view.context.removeAux(idH())
    }
  }
}