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
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Runner.Handler

object ActionRunnerImpl {
  def apply[S <: Sys[S]](obj: Action[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] =
    new Impl(tx.newHandle(obj), h)

  abstract class Base[S <: Sys[S]] extends Runner[S] with ObservableImpl[S, Runner.State] {

    final def factory: Runner.Factory = Runner.Action

    final def tpe: Obj.Type = Action

    final def prepare()(implicit tx: S#Tx): Unit = ???

    final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = ???

    final def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = ???

    final def stop()(implicit tx: S#Tx): Unit = ???

    final def state(implicit tx: S#Tx): Runner.State = ???

    def messages(implicit tx: S#Tx): Any = ???

    def dispose()(implicit tx: S#Tx): Unit = ()
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Obj[S]], val handler: Handler[S])
    extends Base[S] {

    override def toString = s"Runner.Action${hashCode().toHexString}"
  }
}
