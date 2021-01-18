/*
 *  SocketAddress.scala
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

import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, ITargets, Txn}

import scala.util.control.NonFatal

object SocketAddress extends ProductReader[Ex[SocketAddress]] with SocketAddressPlatform {
  def apply(host: Ex[String] = LocalHost(), port: Ex[Int]): Ex[SocketAddress] = Impl(host, port)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[SocketAddress] = {
    require (arity == 2 && adj == 0)
    val _host = in.readEx[String]()
    val _port = in.readEx[Int]()
    SocketAddress(_host, _port)
  }

  object LocalHost extends ProductReader[LocalHost] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): LocalHost = {
      require (arity == 0 && adj == 0)
      new LocalHost()
    }
  }
  final case class LocalHost() extends Ex[String] {
    type Repr[T <: Txn[T]] = IExpr[T, String]

    override def productPrefix: String = s"SocketAddress$$LocalHost" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val value = try {
        mkLocalHostName()
      } catch {
        case NonFatal(_) => "localhost"
      }
      Const(value).expand[T]
    }
  }

  // only used in expansion, so no serialization needed
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

  object Host extends ProductReader[Host] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Host = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[SocketAddress]()
      new Host(_in)
    }
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

  object Port extends ProductReader[Port] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Port = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[SocketAddress]()
      new Port(_in)
    }
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

  // XXX TODO --- what we need this for?
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