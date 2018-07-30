/*
 *  AuralActionImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.synth.proc.Implicits._

import scala.concurrent.stm.Ref

object AuralActionImpl extends AuralObj.Factory {
  type Repr[S <: Sys[S]]  = Action[S]
  def tpe: Obj.Type       = Action

  def apply[S <: SSys[S]](obj: Action[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Action[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH)
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Action[S]])(implicit context: AuralContext[S])
    extends AuralObj.Action[S] with ObservableImpl[S, Runner.State] {

    override def toString = s"AuralAction@${hashCode().toHexString}"

    def tpe: Obj.Type = Action

    private val stateRef = Ref[Runner.State](Runner.Stopped)

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
      // nothing to do. XXX TODO - set state and fire
    }

    def run(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = {
      val oldState = stateRef.swap(Runner.Running)(tx.peer) // XXX TODO fire update
      if (oldState != Runner.Running) {
        val actionObj = objH()
        if (!actionObj.muted) {
          val action    = actionObj
          val universe  = Action.Universe(actionObj, context.workspaceHandle, invoker = None)(context.scheduler.cursor)
          action.execute(universe)
        }
      }
    }

    def stop()(implicit tx: S#Tx): Unit =
      stateRef.set(Runner.Stopped)(tx.peer)

    def state(implicit tx: S#Tx): Runner.State = stateRef.get(tx.peer)

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}