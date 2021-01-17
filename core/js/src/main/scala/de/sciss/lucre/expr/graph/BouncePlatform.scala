/*
 *  BouncePlatform.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.audiofile.AudioFileSpec
import de.sciss.lucre.{Disposable, IExpr, synth}
import de.sciss.proc.impl.BasicRunnerImpl
import de.sciss.proc.{Runner, Universe}
import de.sciss.span.Span

import java.net.URI

trait BouncePlatform {
  protected final class PeerImpl[T <: synth.Txn[T]](obj : IExpr[T, Seq[Obj]],
                                                    out : IExpr[T, URI],
                                                    spec: IExpr[T, AudioFileSpec],
                                                    span: IExpr[T, Span],
                                                   )
                                                   (implicit val universe: Universe[T]) extends BasicRunnerImpl[T] {
    override protected def disposeData()(implicit tx: T): Unit = ()

    override object progress extends Runner.Progress[T] {
      override def current(implicit tx: T): Double = 0.0

      override def react(fun: T => Double => Unit)(implicit tx: T): Disposable[T] = Disposable.empty
    }

    override def prepare(attr: Runner.Attr[T])(implicit tx: T): Unit = ()

    override def run()(implicit tx: T): Unit = {
      println("Bounce: not yet implemented on Scala.js!")
    }

    override def stop()(implicit tx: T): Unit = ()
  }
}
