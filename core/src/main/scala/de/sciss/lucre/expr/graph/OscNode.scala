/*
 *  OscNode.scala
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

import java.net.{InetAddress, InetSocketAddress}

import de.sciss.lucre.event.impl.{IEventImpl, IGenerator}
import de.sciss.lucre.event.{IEvent, IPull, ITargets}
import de.sciss.lucre.expr.impl.IControlImpl
import de.sciss.lucre.expr.{Act, Control, Ex, Graph, IControl, IExpr, ITrigger, Trig}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.{peer => txPeer}
import de.sciss.model.Change
import de.sciss.osc

import scala.concurrent.stm.Ref
import scala.util.control.NonFatal

object OscNode {
  private[graph] final val keyDump         = "dump"
  private[graph] final val keyCodec        = "codec"

  private[graph] final val defaultDump     = osc.Dump.Off.id
  private[graph] final val defaultCodec    = "1.0"

  final case class Dump(n: OscNode) extends Ex[Int] {
    override def productPrefix: String = s"OscNode$$Dump" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, Int] = {
      val valueOpt = ctx.getProperty[Ex[Int]](n, keyDump)
      valueOpt.getOrElse(Constant(defaultDump)).expand[S]
    }
  }

  final case class Codec(n: OscNode) extends Ex[String] {
    override def productPrefix: String = s"OscNode$$Codec" // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, String] = {
      val valueOpt = ctx.getProperty[Ex[String]](n, keyCodec)
      valueOpt.getOrElse(Constant(defaultCodec)).expand[S]
    }
  }
}
sealed trait OscNode extends Control {

//  def port      : Ex[Int]
//  def transport : Ex[String]

  def self: SocketAddress

  def received: Trig

  def packet: Ex[OscPacket]

  def sender: SocketAddress

  /** Possible values: 0 (off), 1 (text), 2 (hex), 3 (both).
    * The dump mode affects both incoming and outgoing packets.
    */
  def dump: Ex[Int] = OscNode.Dump(this)

  def dump_=(x: Ex[Int]): scala.Unit = {
    val b = Graph.builder
    b.putProperty(this, OscNode.keyDump, x)
  }

  /** Describes the OSC tags supported. Possible values: `"1.0"` (OSC specification 1.0),
    * `"1.1"` (OSC specification 1.1), `"1.0d"` (OSC 1.0 plus double precision), `"1.1d"`
    * (OSC 1.1 plus double precision).
    * The codec is always applied bidirectionally for sending and receiving packets.
    */
  def codec: Ex[String] = OscNode.Codec(this)

  def codec_=(x: Ex[String]): scala.Unit = {
    val b = Graph.builder
    b.putProperty(this, OscNode.keyCodec, x)
  }

  //  protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Disposable[S#Tx] = ...
}

object OscUdpNode {
  def apply(localPort: Ex[Int] = Constant(0), localHost: Ex[String] = SocketAddress.LocalHost()): OscUdpNode =
    Impl(localPort, localHost)

  private final class ReceivedExpanded[S <: Sys[S]](peer: Repr[S], tx0: S#Tx)
                                                   (implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IGenerator[S, Unit] {

    peer.received.--->(changed)(tx0)

    def dispose()(implicit tx: S#Tx): Unit =
      peer.received.-/->(changed)

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] =
      Trig.Some

    def changed: IEvent[S, Unit] = this
  }

  final case class Received(n: OscUdpNode) extends Trig {
    override def productPrefix = s"OscUdpNode$$Received"   // serialization

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): ITrigger[S] = {
      val ns = n.expand[S]
      import ctx.targets
      new ReceivedExpanded[S](ns, tx)
    }
  }

  private final class SenderExpanded[S <: Sys[S]](peer: Repr[S], tx0: S#Tx)(implicit protected val targets: ITargets[S])
    extends IExpr[S, (String, Int)] with IEventImpl[S, Change[(String, Int)]] {

    peer.received.--->(changed)(tx0)

    def value(implicit tx: S#Tx): (String, Int) = {
      val s = peer.sender
      (s.getHostString, s.getPort)
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[(String, Int)]] = {
      pull(peer.received).flatMap { ch =>
        val addrBefore  = ch.before._2
        val addrNow     = ch.now   ._2
        val nameBefore  = addrBefore.getHostString
        val portBefore  = addrBefore.getPort
        val nameNow     = addrNow   .getHostString
        val portNow     = addrNow   .getPort
        val chTup       = Change((nameBefore, portBefore), (nameNow, portNow))
        if (chTup.isSignificant) Some(chTup) else None
      }
    }

    def dispose()(implicit tx: S#Tx): Unit =
      peer.received.-/->(changed)

    def changed: IEvent[S, Change[(String, Int)]] = this
  }

  final case class Sender(n: OscUdpNode) extends SocketAddress {
    override def productPrefix = s"OscUdpNode$$Sender"   // serialization

    def host: Ex[String] = ???  // XXX TODO --- should add `_1` ExOp

    def port: Ex[Int] = ???  // XXX TODO --- should add `_2` ExOp

    def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, (String, Int)] = {
      val ns = n.expand[S]
      import ctx.targets
      new SenderExpanded(ns, tx)
    }
  }

  private final class Expanded[S <: Sys[S]](protected val peer: OscUdpNode, localPort: Int, localHost: String)
                                           (implicit protected val targets: ITargets[S],
                                            protected val cursor: stm.Cursor[S])
    extends Repr[S] with IControlImpl[S] with IGenerator[S, Change[(osc.Packet, InetSocketAddress)]] {

    private[this] val lastRef = Ref[(osc.Packet, InetSocketAddress)](
      (osc.Bundle.now(), InetSocketAddress.createUnresolved("invalid", 0)))

    def packet(implicit tx: S#Tx): osc.Packet         = lastRef()._1
    def sender(implicit tx: S#Tx): InetSocketAddress  = lastRef()._2

    def received: IEvent[S, Change[(osc.Packet, InetSocketAddress)]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[(osc.Packet, InetSocketAddress)]] =
      Some(pull.resolve)

    private[this] var addrHaveWarned = false

    private[this] val receiverFun: osc.Receiver.Undirected.Action = { (p, addr) =>
      addr match {
        case iAddr: InetSocketAddress =>
          // XXX TODO --- should we use SoundProcesses.atomic()?
          cursor.step { implicit tx =>
            val tupNow    = (p, iAddr)
            val tupBefore = lastRef.swap(tupNow)
            fire(Change(tupBefore, tupNow))
          }
        case _ =>
          if (!addrHaveWarned) {
            addrHaveWarned = true
            println("OscNode - sender's address is not InetSocketAddress. Dropping packet.")
          }
      }
    }

    @volatile
    private[this] var transmitter : osc.UDP.Transmitter .Undirected = _
    private[this] var receiver    : osc.UDP.Receiver    .Undirected = _

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      tx.afterCommit {
        val t = transmitter
        if (t != null) tryThunk("close")(t.close())
        val r = receiver
        if (r != null) tryThunk("close")(r.close())
      }
    }

    private def tryThunk(op: String)(thunk: => Unit): Unit =
      try {
        thunk
      } catch {
        case NonFatal(ex) =>
          println(s"Could not $op OscUdpNode")
          ex.printStackTrace()
      }

    override def initControl()(implicit tx: S#Tx): Unit = {
      super.initControl()
      tx.afterCommit {
        val t = transmitter
        if (t != null) tryThunk("connect")(t.connect())
        val r = receiver
        if (r != null) tryThunk("connect")(r.connect())
      }
    }

//    private def dump(implicit tx: S#Tx): osc.Dump = dumpRef()

    private def dump_=(value: osc.Dump)(implicit tx: S#Tx): Unit =
      if (dumpRef.swap(value) != value) tx.afterCommit {
        val t = transmitter
        if (t != null) t.dump(value)
        val r = receiver
        if (r != null) r.dump(value)
      }

    private[this] val dumpRef = Ref[osc.Dump](osc.Dump.Off)

    private def dumpModeSafe(id: Int): osc.Dump =
      osc.Dump(math.max(0, math.min(osc.Dump.Both.id, id)))

    def initExpanded()(implicit tx: S#Tx, ctx: Ex.Context[S]): this.type = {
      val codecS: String = ctx.getProperty[Ex[String]](peer, OscNode.keyCodec)
        .fold(OscNode.defaultCodec)(_.expand[S].value)

      val codec: osc.PacketCodec = codecS.toLowerCase match {
        case "1.0"    => osc.PacketCodec().v1_0()
        case "1.1"    => osc.PacketCodec().v1_1()
        case "1.0d"   => osc.PacketCodec().v1_0().doublePrecision()
        case "1.1d"   => osc.PacketCodec().v1_1().doublePrecision()
        case _ =>
          println(s"Warning: OSC codec '$codecS' not supported. Falling back to '1.0'")
          osc.PacketCodec()
      }

      tx.afterCommit {
        val cfgB = osc.UDP.Config()
        cfgB.codec = codec
        tryThunk(s"resolve host $localHost of") {
          cfgB.localAddress = InetAddress.getByName(localHost)
        }
        cfgB.localPort  = localPort
        val cfg         = cfgB.build
        tryThunk("initialize") {
          val t       = osc.UDP.Transmitter(cfg)
          transmitter = t
          val r       = osc.UDP.Receiver(transmitter.channel, cfg)
          r.action    = receiverFun
          receiver    = r
        }
      }

      // after the creation of transmitter, receiver!
      initProperty(OscNode.keyDump, OscNode.defaultDump) { implicit tx => id =>
        dump_=(dumpModeSafe(id))
      }
      this
    }
  }

  private final case class Impl(localPort: Ex[Int], localHost: Ex[String]) extends OscUdpNode {
    override def productPrefix = "OscUdpNode"   // serialization

    def send(target: Ex[SocketAddress], p: Ex[OscPacket]): Act = ???

    def reply(p: Ex[OscPacket]): Act = ???

    def self: SocketAddress = SocketAddress(port = localPort, host = localHost)

    def received: Trig = Received(this)

    def packet: Ex[OscPacket] = ???

    def sender: SocketAddress = Sender(this)

    protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Repr[S] = {
      val localPortV = localPort.expand[S].value
      val localHostV = localHost.expand[S].value
      import ctx.{cursor, targets}
      new Expanded[S](this, localPortV, localHostV).initExpanded()
    }
  }

  trait Repr[S <: Sys[S]] extends IControl[S] {
    def packet(implicit tx: S#Tx): osc.Packet
    def sender(implicit tx: S#Tx): InetSocketAddress

    def received: IEvent[S, Change[(osc.Packet, InetSocketAddress)]]
  }
}
trait OscUdpNode extends OscNode {
  type Repr[S <: Sys[S]] = OscUdpNode.Repr[S]

  def send(target: Ex[SocketAddress], p: Ex[OscPacket]): Act

  def reply(p: Ex[OscPacket]): Act
}

object OscTcpClient {
//  def apply(targetPort: Ex[SocketAddress], targetHost: Ex[String] = ...): OscTcpClient = ...
}
trait OscTcpClient extends OscNode {
  def send(p: Ex[OscPacket]): Act

  var localPort: Ex[Int]
}

object OscTcpServer {
//  def apply(localPort: Ex[Int] = Constant(0), localHost: Ex[String] = ...): OscTcpClient = ...
}
trait OscTcpServer extends OscNode {
  // def sendAll(p: Ex[OscPacket]): Act

  def connected   : Trig
  def disconnected: Trig

  /** Sends to the most recent sender */
  def reply(p: Ex[OscPacket]): Act
}