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
import de.sciss.lucre.expr.impl.{IActionImpl, IControlImpl}
import de.sciss.lucre.expr.{Context, Graph, IAction, IControl, IExpr, ITrigger}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.{peer => txPeer}
import de.sciss.model.Change
import de.sciss.osc

import scala.concurrent.stm.Ref
import scala.util.control.NonFatal

object OscNode {
  private[lucre] final val keyDump         = "dump"
  private[lucre] final val keyCodec        = "codec"

  private[lucre] final val defaultDump     = osc.Dump.Off.id
  private[lucre] final val defaultCodec    = "1.0"

  final case class Dump(n: OscNode) extends Ex[Int] {
    type Repr[S <: Sys[S]] = IExpr[S, Int]

    override def productPrefix: String = s"OscNode$$Dump" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val valueOpt = ctx.getProperty[Ex[Int]](n, keyDump)
      valueOpt.getOrElse(Const(defaultDump)).expand[S]
    }
  }

  final case class Codec(n: OscNode) extends Ex[String] {
    type Repr[S <: Sys[S]] = IExpr[S, String]

    override def productPrefix: String = s"OscNode$$Codec" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val valueOpt = ctx.getProperty[Ex[String]](n, keyCodec)
      valueOpt.getOrElse(Const(defaultCodec)).expand[S]
    }
  }
}
sealed trait OscNode extends Control {

//  def port      : Ex[Int]
//  def transport : Ex[String]

  def self: Ex[SocketAddress]

  def received: Trig

//  def packet: Ex[OscPacket]
  def message: Ex[OscMessage]

  def sender: Ex[SocketAddress]

  /** Possible values: 0 (off), 1 (text), 2 (hex), 3 (both).
    * The dump mode affects both incoming and outgoing packets.
    */
  def dump: Ex[Int] = OscNode.Dump(this)

  def dump_=(x: Ex[Int]): scala.Unit = {
    val b = Graph.builder
    b.putProperty(this, OscNode.keyDump, x)
  }

  def select(name: Ex[String], args: CaseDef[_]*): Trig = {
    val r = received
    val m = message
    val s = m.select(name, args: _*)
    r ---> s
    s
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

  //  protected def mkControl[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Disposable[S#Tx] = ...
}

object OscUdpNode {
  def apply(localPort: Ex[Int] = Const(0), localHost: Ex[String] = SocketAddress.LocalHost()): OscUdpNode =
    Impl(localPort, localHost)

  private final class ReceivedExpanded[S <: Sys[S]](peer: Repr[S], tx0: S#Tx)
                                                   (implicit protected val targets: ITargets[S])
    extends ITrigger[S] with IEventImpl[S, Unit] {

    peer.received.--->(changed)(tx0)

    def dispose()(implicit tx: S#Tx): Unit =
      peer.received.-/->(changed)

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Unit] =
      Trig.Some

    def changed: IEvent[S, Unit] = this
  }

  final case class Received(n: OscUdpNode) extends Trig {
    type Repr[S <: Sys[S]] = ITrigger[S]

    override def productPrefix = s"OscUdpNode$$Received"   // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val ns = n.expand[S]
      import ctx.targets
      new ReceivedExpanded[S](ns, tx)
    }
  }

  private final class SenderExpanded[S <: Sys[S]](peer: Repr[S], tx0: S#Tx)
                                                 (implicit protected val targets: ITargets[S])
    extends IExpr[S, SocketAddress] with IEventImpl[S, Change[SocketAddress]] {

    peer.received.--->(changed)(tx0)

    def value(implicit tx: S#Tx): SocketAddress = {
      val s = peer.sender
      SocketAddress(s.getHostString, s.getPort)
    }

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[SocketAddress]] =
      pull(peer.received) match {
        case Some(ch) if ch.before._2 != ch.now._2 =>
          val addrBefore  = ch.before._2
          val addrNow     = ch.now   ._2
          val nameBefore  = addrBefore.getHostString
          val portBefore  = addrBefore.getPort
          val nameNow     = addrNow   .getHostString
          val portNow     = addrNow   .getPort
          val chTup       = Change(SocketAddress(nameBefore, portBefore), SocketAddress(nameNow, portNow))
          if (chTup.isSignificant) Some(chTup) else None

        case _ => None
      }

    def dispose()(implicit tx: S#Tx): Unit =
      peer.received.-/->(changed)

    def changed: IEvent[S, Change[SocketAddress]] = this
  }

  final case class Sender(n: OscUdpNode) extends Ex[SocketAddress] {
    type Repr[S <: Sys[S]] = IExpr[S, SocketAddress]

    override def productPrefix = s"OscUdpNode$$Sender"   // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val ns = n.expand[S]
      import ctx.targets
      new SenderExpanded(ns, tx)
    }
  }

  private final class MessageExpanded[S <: Sys[S]](peer: Repr[S], tx0: S#Tx)(implicit protected val targets: ITargets[S])
    extends IExpr[S, OscMessage] with IEventImpl[S, Change[OscMessage]] {

    peer.received.--->(changed)(tx0)

    private def convert(m: osc.Message): OscMessage =
      new OscMessage(m.name, m.args: _*)

    def value(implicit tx: S#Tx): OscMessage =
      convert(peer.message)

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[OscMessage]] =
      pull(peer.received) match {
        case Some(ch) =>
          val mBefore = convert(ch.before ._1)
          val mNow    = convert(ch.now    ._1)
          val chNew   = Change(mBefore, mNow)
          if (chNew.isSignificant) Some(chNew) else None

        case None => None
      }

    def dispose()(implicit tx: S#Tx): Unit =
      peer.received.-/->(changed)

    def changed: IEvent[S, Change[OscMessage]] = this
  }

  final case class Message(n: OscUdpNode) extends Ex[OscMessage] {
    type Repr[S <: Sys[S]] = IExpr[S, OscMessage]

    override def productPrefix = s"OscUdpNode$$Message"   // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new MessageExpanded(n.expand[S], tx)
    }
  }

  private final class Expanded[S <: Sys[S]](protected val peer: OscUdpNode, localPort: Int, localHost: String)
                                           (implicit protected val targets: ITargets[S],
                                            protected val cursor: stm.Cursor[S])
    extends Repr[S] with IControlImpl[S] with IGenerator[S, Change[(osc.Message, InetSocketAddress)]] {

    private[this] val dummySocket = InetSocketAddress.createUnresolved("invalid", 0)

    private[this] val lastRcvRef = Ref[(osc.Message, InetSocketAddress)](
      (osc.Message(""), dummySocket))

    private[this] val lastTrnsRef = Ref[(SocketAddress, InetSocketAddress)]((SocketAddress("invalid", 0), dummySocket))

    def message (implicit tx: S#Tx): osc.Message        = lastRcvRef()._1
    def sender  (implicit tx: S#Tx): InetSocketAddress  = lastRcvRef()._2

    def received: IEvent[S, Change[(osc.Message, InetSocketAddress)]] = this

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx): Option[Change[(osc.Message, InetSocketAddress)]] =
      Some(pull.resolve)

    private[this] var addrHaveWarned = false

    private[this] val receiverFun: osc.Receiver.Undirected.Action = { (p, addr) =>
      addr match {
        case iAddr: InetSocketAddress =>
          // XXX TODO --- should we use SoundProcesses.atomic()?
          cursor.step { implicit tx =>
            def fire1(m: osc.Message): Unit = {
              val tupNow    = (m, iAddr)
              val tupBefore = lastRcvRef.swap(tupNow)
              fire(Change(tupBefore, tupNow))
            }

            def fireAll(pp: osc.Packet): Unit =
              p match {
                case m: osc.Message => fire1(m)
                case b: osc.Bundle  =>
                  b.packets.foreach(fireAll)
              }

            fireAll(p)
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

    def send(target: SocketAddress, p: OscPacket)(implicit tx: S#Tx): Unit =
      tx.afterCommit {
        sendNow(target, p)
      }

    private def sendWith(target: InetSocketAddress, p: OscPacket): Unit = {
      val p1: osc.Packet = p match {
        case m: OscMessage =>
          osc.Message(m.name, m.args: _*) // XXX TODO --- adjust args to codec

//        case _: OscBundle => ...
      }
      tryThunk("send from") {
        val t = transmitter
        if (t != null) t.send(p1, target)
      }
    }

    private def sendNow(target: SocketAddress, p: OscPacket): Unit = {
      val (lastTarget, lastSck) = lastTrnsRef.single.get
      if (lastTarget == target) sendWith(lastSck, p)
      else tryThunk("resolve target in") {
        val res = new InetSocketAddress(target.host, target.port)
        lastTrnsRef.single.set((target, res))
        sendWith(res, p)
      }
    }

    override def dispose()(implicit tx: S#Tx): Unit = {
      super.dispose()
      tx.afterCommit {
        // both share the same `SocketChannel`
        // -- I think we get less warnings if we close the receiver first
        val r = receiver
        if (r != null) tryThunk("close")(r.close())
        val t = transmitter
        if (t != null) tryThunk("close")(t.close())
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

    private[this] var codec: osc.PacketCodec = _

    override def initControl()(implicit tx: S#Tx): Unit = {
      super.initControl()

      // only create the transmitter and receiver here,
      // as the constructors allocate the socket channel
      // (even before `connect` which is actually a dummy here
      // as no hard-wired targets are used).
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
          t.connect()
          transmitter = t
          val r       = osc.UDP.Receiver(transmitter.channel, cfg)
          r.action    = receiverFun
          r.connect()
          receiver    = r
        }
      }

//      tx.afterCommit {
//        val t = transmitter
//        if (t != null) tryThunk("connect")(t.connect())
//        val r = receiver
//        if (r != null) tryThunk("connect")(r.connect())
//      }
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

    def initExpanded()(implicit tx: S#Tx, ctx: Context[S]): this.type = {
      val codecS: String = ctx.getProperty[Ex[String]](peer, OscNode.keyCodec)
        .fold(OscNode.defaultCodec)(_.expand[S].value)

      codec = codecS.toLowerCase match {
        case "1.0"    => osc.PacketCodec().scsynth() // automatically encodes Long and Double in single precision
        case "1.1"    => osc.PacketCodec().v1_1()
        case "1.0d"   => osc.PacketCodec().v1_0().booleansAsInts().doublePrecision()
        case "1.1d"   => osc.PacketCodec().v1_1().doublePrecision()
        case _ =>
          println(s"Warning: OSC codec '$codecS' not supported. Falling back to '1.0'")
          osc.PacketCodec()
      }

      initProperty(OscNode.keyDump, OscNode.defaultDump) { implicit tx => id =>
        dump_=(dumpModeSafe(id))
      }
      this
    }
  }

  private final class SendExpanded[S <: Sys[S]](peer: Repr[S], target: IExpr[S, SocketAddress],
                                                p: IExpr[S, OscPacket], tx0: S#Tx)
    extends IActionImpl[S] {

    private[this] val targetRef = Ref(target.value(tx0))

    // under the assumption that `target` rarely or never changes, we cache it here
    // to avoid having to call `target.value` for every `executeAction`
    addDisposable(target.changed.react { implicit tx => ch =>
      targetRef() = ch.now
    } (tx0))(tx0)

    def executeAction()(implicit tx: S#Tx): Unit = {
      val pv = p.value
      peer.send(targetRef(), pv)
    }
  }

  final case class Send(n: OscUdpNode, target: Ex[SocketAddress], p: Ex[OscPacket]) extends Act {
    type Repr[S <: Sys[S]] = IAction[S]

    override def productPrefix = s"OscUdpNode$$Send"   // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new SendExpanded[S](n.expand[S], target.expand, p.expand, tx)
  }

  private final case class Impl(localPort: Ex[Int], localHost: Ex[String]) extends OscUdpNode {
    override def productPrefix = "OscUdpNode"   // serialization

    def send(target: Ex[SocketAddress], p: Ex[OscPacket]): Act = Send(this, target, p)

//    def reply(p: Ex[OscPacket]): Act = ...

    def self: Ex[SocketAddress] = SocketAddress(port = localPort, host = localHost)

    def received: Trig              = Received(this)
    def sender  : Ex[SocketAddress] = Sender  (this)
    def message : Ex[OscMessage]    = Message (this)

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val localPortV = localPort.expand[S].value
      val localHostV = localHost.expand[S].value
      import ctx.{cursor, targets}
      new Expanded[S](this, localPortV, localHostV).initExpanded()
    }
  }

  trait Repr[S <: Sys[S]] extends IControl[S] {
    def message(implicit tx: S#Tx): osc.Message
    def sender (implicit tx: S#Tx): InetSocketAddress

    def send(target: SocketAddress, p: OscPacket)(implicit tx: S#Tx): Unit

    def received: IEvent[S, Change[(osc.Message, InetSocketAddress)]]
  }
}
trait OscUdpNode extends OscNode {
  type Repr[S <: Sys[S]] = OscUdpNode.Repr[S]

  def send(target: Ex[SocketAddress], p: Ex[OscPacket]): Act

//  /** Not yet implemented! */
//  def reply(p: Ex[OscPacket]): Act
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