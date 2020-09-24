/*
 *  SocketAddress.scala
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

import java.net.InetAddress

import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, ITargets, Txn}

import scala.util.control.NonFatal

object SocketAddress {
  def apply(host: Ex[String] = LocalHost(), port: Ex[Int]): Ex[SocketAddress] = Impl(host, port)
  
  final case class LocalHost() extends Ex[String] {
    type Repr[T <: Txn[T]] = IExpr[T, String]

    override def productPrefix: String = s"SocketAddress$$LocalHost" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val value = try {
        InetAddress.getLocalHost.getHostName
      } catch {
        case NonFatal(_) => "localhost"
      }
      Const(value).expand[T]
    }
  }

  final case class Apply private[lucre] () extends BinaryOp.Op[String, Int, SocketAddress] {
    def apply(host: String, port: Int): SocketAddress =
      SocketAddress(host, port)

    override def productPrefix: String = s"SocketAddress$$$name"

    def name: String = "Apply"
  }

  private final class HostExpanded[T <: Txn[T]](in: IExpr[T, SocketAddress], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, SocketAddress, String](in, tx0) {

    protected def mapValue(inValue: SocketAddress)(implicit tx: T): String = inValue.host
  }

  final case class Host(in: Ex[SocketAddress]) extends Ex[String] {
    override def productPrefix: String = s"SocketAddress$$Host" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, String]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new HostExpanded(in.expand[T], tx)
    }
  }

  private final class PortExpanded[T <: Txn[T]](in: IExpr[T, SocketAddress], tx0: T)(implicit targets: ITargets[T])
    extends MappedIExpr[T, SocketAddress, Int](in, tx0) {

    protected def mapValue(inValue: SocketAddress)(implicit tx: T): Int = inValue.port
  }

  final case class Port(in: Ex[SocketAddress]) extends Ex[Int] {
    override def productPrefix: String = s"SocketAddress$$Port" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Int]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new PortExpanded(in.expand[T], tx)
    }
  }

  implicit final class ExOps(private val x: Ex[SocketAddress]) extends AnyVal {
    def host: Ex[String]  = SocketAddress.Host(x)
    def port: Ex[Int]     = SocketAddress.Port(x)
  }

  implicit object ExValue extends Ex.Value[SocketAddress]

  private final case class Impl(host: Ex[String], port: Ex[Int]) extends Ex[SocketAddress] {
    type Repr[T <: Txn[T]] = IExpr[T, SocketAddress]

    override def productPrefix: String = "SocketAddress" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val hostEx = host.expand[T]
      val portEx = port.expand[T]
      new BinaryOp.Expanded(Apply(), hostEx, portEx, tx)
    }
  }
}
final case class SocketAddress private[lucre] (host: String, port: Int)