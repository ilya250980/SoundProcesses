/*
 *  ProcRunnerImpl.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Runner.Handler

import scala.concurrent.stm.Ref

object ProcRunnerImpl {
  def apply[S <: Sys[S]](obj: Proc[S], h: Handler[S])(implicit tx: S#Tx): Runner[S] = {
    val t = h.mkTransport()
    t.addObject(obj)
    new Impl(tx.newHandle(obj), t, h).init(obj)
  }

  private final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, Proc[S]],
                                        t: Transport[S],
                                        val handler: Handler[S])
    extends BasicRunnerImpl[S] {

    override def toString = s"Runner.Proc${hashCode().toHexString}"

    private[this] val targetState = Ref[Runner.State](Runner.Stopped)

    def factory: Runner.Factory = Runner.Proc

    def init(obj: Proc[S])(implicit tx: S#Tx): this.type = {
      t.getView(obj).foreach(auralViewAdded)
      this
    }

    private def auralViewAdded(v: AuralObj[S])(implicit tx: S#Tx): Unit = {
      val ts = targetState()
      val vs = v.state
      if (vs != ts) ts match {
        case Runner.Stopped   => v.stop()
        case Runner.Preparing => v.prepare(TimeRef.undefined)
        case Runner.Running   => v.run    (TimeRef.undefined, ())
        case _ =>
      }
    }

    def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = ???

    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: S#Tx): Unit = ???

    def stop()(implicit tx: S#Tx): Unit = ???

    protected def disposeData()(implicit tx: S#Tx): Unit = {
      t.dispose()
    }

    def messages(implicit tx: S#Tx): Any = ???
  }
}