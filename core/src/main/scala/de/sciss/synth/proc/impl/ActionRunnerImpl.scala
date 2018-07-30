/*
 *  ActionRunnerImpl.scala
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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Cursor, Obj}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Runner.{Handler, Prepared, Running, Stopped}

import scala.concurrent.stm.Ref

object ActionRunnerImpl {
  def apply[S <: Sys[S]](obj: Action[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
    new Impl(tx.newHandle(obj), h)

  abstract class Base[S <: stm.Sys[S], Target] extends ViewBase[S, Target] with ObservableImpl[S, Runner.State] {
    protected def workspace : WorkspaceHandle [S]
    protected def cursor    : Cursor          [S]

    override def objH: stm.Source[S#Tx, Action[S]]

    private[this] val stateRef = Ref[Runner.State](Runner.Stopped)

    final def factory: Runner.Factory = Runner.Action

    final def tpe: Obj.Type = Action

    final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit =
      state = Prepared

    final def stop()(implicit tx: S#Tx): Unit =
      state = Stopped

    final def state(implicit tx: S#Tx): Runner.State = stateRef()

    final protected def state_=(now: Runner.State)(implicit tx: S#Tx): Unit = {
      val before = stateRef.swap(now)
      if (before != now) fire(now)
    }

    def messages(implicit tx: S#Tx): Any = ???

    def dispose()(implicit tx: S#Tx): Unit = ()

    def run(timeRef: TimeRef.Option, target: Target)(implicit tx: S#Tx): Unit = {
      state = Running
      val action = objH()
      if (!action.muted) {
        val universe = Action.Universe[S](action, workspace)(cursor)
        action.execute(universe)
      }
      state = Stopped
    }
  }

  private final class Impl[S <: stm.Sys[S]](val objH: stm.Source[S#Tx, Action[S]], val handler: Handler[S])
    extends Base[S, Unit] with Runner[S] {

    protected def workspace : WorkspaceHandle [S] = handler.workspace
    protected def cursor    : Cursor          [S] = handler.cursor

    override def toString = s"Runner.Action${hashCode().toHexString}"
  }
}
