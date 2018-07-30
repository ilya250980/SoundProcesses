/*
 *  Runner.scala
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

package de.sciss.lucre.swing
package graph

import de.sciss.lucre.expr.{Act, Control, Ex, IAction}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth
import de.sciss.synth.proc
import de.sciss.synth.proc.Runner.Handler
import de.sciss.synth.proc.TimeRef
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}

object Runner {
  object Run {
    private final class Expanded[S <: Sys[S]](r: proc.Runner[S]) extends IAction[S] {
      def execute()(implicit tx: S#Tx): Unit =
        r.run(TimeRef.undefined, ())

      def dispose()(implicit tx: S#Tx): Unit = ()
    }
  }
  final case class Run(r: Runner) extends Act {
    override def productPrefix: String = s"Runner$$Run" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IAction[S] = {
      val rx = r.expand[S]
      new Run.Expanded[S](rx)
    }
  }

  object Stop {
    private final class Expanded[S <: Sys[S]](r: proc.Runner[S]) extends IAction[S] {
      def execute()(implicit tx: S#Tx): Unit =
        r.stop()

      def dispose()(implicit tx: S#Tx): Unit = ()
    }
  }
  final case class Stop(r: Runner) extends Act {
    override def productPrefix: String = s"Runner$$Stop" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IAction[S] = {
      val rx = r.expand[S]
      new Stop.Expanded[S](rx)
    }
  }
}
final case class Runner(key: String) extends Control {

  type Repr[S <: Sys[S]] = proc.Runner[S]

  def run : Act = Runner.Run  (this)
  def stop: Act = Runner.Stop (this)

  protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] =
    tx.system match {
      case _: synth.Sys[_] =>
        // XXX TODO --- ugly ugly ugly
        mkControlImpl[synth.NoSys](ctx.asInstanceOf[Ex.Context[synth.NoSys]], tx.asInstanceOf[synth.NoSys#Tx])
          .asInstanceOf[Repr[S]]

      case _ => throw new Exception("Need a SoundProcesses system")
    }

  private def mkControlImpl[S <: synth.Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] = {
    import ctx.{cursor, workspace}
    val objOpt                  = ctx.selfOption.flatMap(self => self.attr.get(key))
    val obj                     = objOpt.getOrElse(throw UGB.MissingIn(UGB.AttributeKey(key)))
    implicit val h: Handler[S]  = Handler[S]()
    val runOpt                  = proc.Runner[S](obj)
    runOpt.getOrElse(throw new Exception(s"No runner for ${obj.tpe}"))
  }
}
