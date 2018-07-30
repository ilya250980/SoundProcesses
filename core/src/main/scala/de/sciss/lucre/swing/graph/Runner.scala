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
import de.sciss.lucre.stm.{Cursor, Disposable, Sys, WorkspaceHandle}

object Runner {
  final case class Run(r: Runner) extends Act {
    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IAction[S] = ???
  }

  final case class Stop(r: Runner) extends Act {
    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IAction[S] = ???
  }

  private final class Expanded[S <: Sys[S]](key: String)(implicit workspace: WorkspaceHandle[S], cursor: Cursor[S])
    extends Disposable[S#Tx] {

    def init()(implicit tx: S#Tx): this.type = {
      ??? // val h = Handler[S]()
      this
    }

    def dispose()(implicit tx: S#Tx): Unit = ???
  }
}
final case class Runner(key: String) extends Control {

  type Repr[S <: Sys[S]] = Disposable[S#Tx]

  def run : Act = Runner.Run  (this)
  def stop: Act = Runner.Stop (this)

  protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] = {
    import ctx.{cursor, workspace}
    new Runner.Expanded[S](key).init()
  }
}
