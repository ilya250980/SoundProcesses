/*
 *  SocketAddress.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import java.net.InetAddress

import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys

import scala.util.control.NonFatal

object SocketAddress {
  def apply(host: Ex[String] = LocalHost(), port: Ex[Int]): Ex[SocketAddress] = Impl(host, port)

  final case class LocalHost() extends Ex[String] {
    type Repr[S <: Sys[S]] = IExpr[S, String]

    override def productPrefix: String = s"SocketAddress$$LocalHost" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val value = try {
        InetAddress.getLocalHost.getHostName
      } catch {
        case NonFatal(_) => "localhost"
      }
      Const(value).expand[S]
    }
  }

  final case class Apply private[lucre] () extends BinaryOp.Op[String, Int, SocketAddress] {
    def apply(host: String, port: Int): SocketAddress =
      SocketAddress(host, port)

    override def productPrefix: String = s"SocketAddress$$$name"

    def name: String = "Apply"
  }

  private final case class Impl(host: Ex[String], port: Ex[Int]) extends Ex[SocketAddress] {
    type Repr[S <: Sys[S]] = IExpr[S, SocketAddress]

    override def productPrefix: String = "SocketAddress" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      val hostEx = host.expand[S]
      val portEx = port.expand[S]
      new BinaryOp.Expanded(Apply(), hostEx, portEx, tx)
    }
  }
}
final case class SocketAddress private[lucre] (host: String, port: Int)