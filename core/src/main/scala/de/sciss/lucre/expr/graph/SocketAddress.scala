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

import de.sciss.lucre.expr.{Ex, ExTuple2, IExpr}
import de.sciss.lucre.stm.Sys

import scala.util.control.NonFatal

object SocketAddress {
  def apply(host: Ex[String], port: Ex[Int]): SocketAddress = Impl(host, port)

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

  private final case class Impl(host: Ex[String], port: Ex[Int]) extends SocketAddress {
    override def productPrefix: String = "SocketAddress" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, (String, Int)] =
      ExTuple2(host, port).expand[S]
  }
}
// XXX TODO --- should it be a non-ex, and we handle `Ex[SocketAddress]` like `Ex[OscMessage]` ?
trait SocketAddress extends Ex[(String, Int)] {
  def host: Ex[String]
  def port: Ex[Int]
}
