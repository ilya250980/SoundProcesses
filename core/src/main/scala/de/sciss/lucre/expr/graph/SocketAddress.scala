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

import de.sciss.lucre.aux.Aux
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys

import scala.util.control.NonFatal

object SocketAddress {
  def apply(host: Ex[String] = LocalHost(), port: Ex[Int]): Ex[SocketAddress] = Impl(host, port)

  final case class LocalHost() extends Ex[String] {
    override def productPrefix: String = s"SocketAddress$$LocalHost" // serialization

    def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, String] = {
      val value = try {
        InetAddress.getLocalHost.getHostName
      } catch {
        case NonFatal(_) => "localhost"
      }
      Const(value).expand[S]
    }
  }

  final case class Apply private[graph] () extends BinaryOp.Op[String, Int, SocketAddress] {
    def apply(host: String, port: Int): SocketAddress =
      SocketAddress(host, port)

    override def productPrefix: String = s"SocketAddress$$$name"

    def name: String = "Apply"

    def aux: List[Aux] = Nil
  }

  private final case class Impl(host: Ex[String], port: Ex[Int]) extends Ex[SocketAddress] {
    override def productPrefix: String = "SocketAddress" // serialization

    def expand[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): IExpr[S, SocketAddress] = {
      import ctx.targets
      val hostEx = host.expand[S]
      val portEx = port.expand[S]
      new BinaryOp.Expanded(Apply(), hostEx, portEx, tx)
    }
  }
}
final case class SocketAddress private[graph] (host: String, port: Int)