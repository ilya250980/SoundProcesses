/*
 *  SysPlatform.scala
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

package de.sciss.lucre.expr.graph

import java.net.URI

import de.sciss.lucre.expr.graph.Sys.Process.Peer
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.{Disposable, IExpr, ITargets, Txn}
import de.sciss.synth.proc.{Runner, Universe}

trait SysPlatform {
  protected final class ExpandedExit[T <: Txn[T]](code: IExpr[T, Int]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit = {
      tx.afterCommit {
        Console.err.println("Sys.Exit: unsupported on .js")
      }
    }
  }

  protected final class ExpandedProcess[T <: Txn[T]](cmd: IExpr[T, String], args: IExpr[T, Seq[String]],
                                                     dirOpt: Option[IExpr[T, URI]])
                                                    (implicit val universe: Universe[T], targets: ITargets[T])
    extends Peer[T] {

    def output: IExpr[T, String] = new Const.Expanded("")

    object messages extends Runner.Messages[T] {
      def current(implicit tx: T): List[Runner.Message] = Nil

      def react(fun: T => List[Runner.Message] => Unit)(implicit tx: T): Disposable[T] = Disposable.empty
    }

    object progress extends Runner.Progress[T] {
      def current(implicit tx: T): Double = 0.0

      def react(fun: T => Double => Unit)(implicit tx: T): Disposable[T] = Disposable.empty
    }

    def prepare(attr: Runner.Attr[T])(implicit tx: T): Unit = ()

    def run()(implicit tx: T): Unit = ()

    def initControl()(implicit tx: T): Unit = ()

    def state(implicit tx: T): Runner.State = Runner.Stopped

    def stop()(implicit tx: T): Unit = ()

    def dispose()(implicit tx: T): Unit = ()

    def react(fun: T => Runner.State => Unit)(implicit tx: T): Disposable[T] = Disposable.empty
  }
}
