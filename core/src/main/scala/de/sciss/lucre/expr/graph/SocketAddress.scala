/*
 *  SocketAddress.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import java.net.InetAddress

import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.event.impl.IEventImpl
import de.sciss.lucre.expr.{Ex, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.model.Change

import scala.util.control.NonFatal

object SocketAddress {
  def apply(host: Ex[String] = LocalHost(), port: Ex[Int]): Ex[SocketAddress] = Impl(host, port)

  final case class LocalHost() extends Ex[String] {
    override def productPrefix: String = s"SocketAddress$$LocalHost" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, String] = {
      val value = try {
        InetAddress.getLocalHost.getHostName
      } catch {
        case NonFatal(_) => "localhost"
      }
      Constant(value).expand[S]
    }
  }

  private final class Expanded[S <: Sys[S]](host: IExpr[S, String], port: IExpr[S, Int], tx0: S#Tx)
                                           (implicit protected val targets: ITargets[S])
    extends IExpr[S, SocketAddress] with IEventImpl[S, Change[SocketAddress]] {

    host.changed.--->(changed)(tx0)
    port.changed.--->(changed)(tx0)

    def value(implicit tx: S#Tx): SocketAddress = {
      val hostV = host.value
      val portV = port.value
      new SocketAddress(hostV, portV)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      host.changed.-/->(changed)
      port.changed.-/->(changed)
    }

    def changed: IEvent[S, Change[SocketAddress]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[SocketAddress]] = {
      val hostEvt = host.changed
      val portEvt = port.changed
      val hostOpt = if (pull.contains(hostEvt)) pull(hostEvt) else None
      val portOpt = if (pull.contains(portEvt)) pull(portEvt) else None
      val hostCh  = hostOpt.getOrElse {
        val hostV = host.value
        Change(hostV, hostV)
      }
      val portCh = portOpt.getOrElse {
        val portV = port.value
        Change(portV, portV)
      }
      val mBefore = new SocketAddress(hostCh.before, portCh.before)
      val mNow    = new SocketAddress(hostCh.now   , portCh.now   )
      val ch      = Change(mBefore, mNow)

      if (ch.isSignificant) Some(ch) else None
    }
  }

  private final case class Impl(host: Ex[String], port: Ex[Int]) extends Ex[SocketAddress] {
    override def productPrefix: String = "SocketAddress" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, SocketAddress] = {
      import ctx.targets
      val hostEx = host.expand[S]
      val portEx = port.expand[S]
      new Expanded(hostEx, portEx, tx)
    }
  }
}
final case class SocketAddress private[graph] (host: String, port: Int)