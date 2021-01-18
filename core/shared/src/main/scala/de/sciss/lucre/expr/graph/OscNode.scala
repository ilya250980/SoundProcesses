/*
 *  OscNode.scala
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

import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, Graph, IAction, IControl, ITrigger}
import de.sciss.lucre.impl.{IChangeEventImpl, IEventImpl}
import de.sciss.lucre.{IChangeEvent, IEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.osc

import scala.concurrent.stm.Ref

object OscNode {
  private[lucre] final val keyDump         = "dump"
  private[lucre] final val keyCodec        = "codec"

  private[lucre] final val defaultDump     = osc.Dump.Off.id
  private[lucre] final val defaultCodec    = "1.0"

  object Dump extends ProductReader[Dump] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Dump = {
      require (arity == 1 && adj == 0)
      val _n = in.readProductT[OscNode]()
      new Dump(_n)
    }
  }
  final case class Dump(n: OscNode) extends Ex[Int] {
    type Repr[T <: Txn[T]] = IExpr[T, Int]

    override def productPrefix: String = s"OscNode$$Dump" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val valueOpt = ctx.getProperty[Ex[Int]](n, keyDump)
      valueOpt.getOrElse(Const(defaultDump)).expand[T]
    }
  }

  object Codec extends ProductReader[Codec] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Codec = {
      require (arity == 1 && adj == 0)
      val _n = in.readProductT[OscNode]()
      new Codec(_n)
    }
  }
  final case class Codec(n: OscNode) extends Ex[String] {
    type Repr[T <: Txn[T]] = IExpr[T, String]

    override def productPrefix: String = s"OscNode$$Codec" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val valueOpt = ctx.getProperty[Ex[String]](n, keyCodec)
      valueOpt.getOrElse(Const(defaultCodec)).expand[T]
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

  //  protected def mkControl[T <: Txn[T]](implicit ctx: Context[T], tx: T): Disposable[T] = ...
}

object OscUdpNode extends ProductReader[OscUdpNode] with OscUdpNodePlatform {
  def apply(localPort: Ex[Int] = Const(0), localHost: Ex[String] = SocketAddress.LocalHost()): OscUdpNode =
    Impl(localPort, localHost)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): OscUdpNode = {
    require (arity == 2 && adj == 0)
    val _localPort  = in.readEx[Int]()
    val _localHost  = in.readEx[String]()
    OscUdpNode(_localPort, _localHost)
  }

  private final class ReceivedExpanded[T <: Txn[T]](peer: Repr[T], tx0: T)
                                                   (implicit protected val targets: ITargets[T])
    extends ITrigger[T] with IEventImpl[T, Unit] {

    peer.received.--->(changed)(tx0)

    def dispose()(implicit tx: T): Unit =
      peer.received.-/->(changed)

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] =
      Trig.Some

    def changed: IEvent[T, Unit] = this
  }

  object Received extends ProductReader[Received] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Received = {
      require (arity == 1 && adj == 0)
      val _n = in.readProductT[OscUdpNode]()
      new Received(_n)
    }
  }
  final case class Received(n: OscUdpNode) extends Trig {
    type Repr[T <: Txn[T]] = ITrigger[T]

    override def productPrefix = s"OscUdpNode$$Received"   // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val ns = n.expand[T]
      import ctx.targets
      new ReceivedExpanded[T](ns, tx)
    }
  }

  object Sender extends ProductReader[Sender] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Sender = {
      require (arity == 1 && adj == 0)
      val _n = in.readProductT[OscUdpNode]()
      new Sender(_n)
    }
  }
  final case class Sender(n: OscUdpNode) extends Ex[SocketAddress] {
    type Repr[T <: Txn[T]] = IExpr[T, SocketAddress]

    override def productPrefix = s"OscUdpNode$$Sender"   // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val ns = n.expand[T]
      import ctx.targets
      new SenderExpanded(ns, tx)
    }
  }

  private final class MessageExpanded[T <: Txn[T]](peer: Repr[T], tx0: T)(implicit protected val targets: ITargets[T])
    extends IExpr[T, OscMessage] with IChangeEventImpl[T, OscMessage] {

    peer.received.--->(changed)(tx0)

    private def convert(m: osc.Message): OscMessage =
      new OscMessage(m.name, m.args: _*)

    def value(implicit tx: T): OscMessage =
      convert(peer.message)

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): OscMessage = {
      val (m, _) = pull.applyChange(peer.received)
      convert(m)
    }

    def dispose()(implicit tx: T): Unit =
      peer.received.-/->(changed)

    def changed: IChangeEvent[T, OscMessage] = this
  }

  object Message extends ProductReader[Message] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Message = {
      require (arity == 1 && adj == 0)
      val _n = in.readProductT[OscUdpNode]()
      new Message(_n)
    }
  }
  final case class Message(n: OscUdpNode) extends Ex[OscMessage] {
    type Repr[T <: Txn[T]] = IExpr[T, OscMessage]

    override def productPrefix = s"OscUdpNode$$Message"   // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new MessageExpanded(n.expand[T], tx)
    }
  }

  private final class SendExpanded[T <: Txn[T]](peer: Repr[T], target: IExpr[T, SocketAddress],
                                                p: IExpr[T, OscPacket], tx0: T)
    extends IActionImpl[T] {

    private[this] val targetRef = Ref(target.value(tx0))

    // under the assumption that `target` rarely or never changes, we cache it here
    // to avoid having to call `target.value` for every `executeAction`
    addDisposable(target.changed.react { implicit tx => ch =>
      targetRef() = ch.now
    } (tx0))(tx0)

    def executeAction()(implicit tx: T): Unit = {
      val pv = p.value
      peer.send(targetRef(), pv)
    }
  }

  object Send extends ProductReader[Send] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Send = {
      require (arity == 1 && adj == 0)
      val _n      = in.readProductT[OscUdpNode]()
      val _target = in.readEx[SocketAddress]()
      val _p      = in.readEx[OscPacket]()
      new Send(_n, _target, _p)
    }
  }
  final case class Send(n: OscUdpNode, target: Ex[SocketAddress], p: Ex[OscPacket]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix = s"OscUdpNode$$Send"   // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new SendExpanded[T](n.expand[T], target.expand, p.expand, tx)
  }

  private final case class Impl(localPort: Ex[Int], localHost: Ex[String]) extends OscUdpNode {
    override def productPrefix = "OscUdpNode"   // serialization

    def send(target: Ex[SocketAddress], p: Ex[OscPacket]): Act = Send(this, target, p)

//    def reply(p: Ex[OscPacket]): Act = ...

    def self: Ex[SocketAddress] = SocketAddress(port = localPort, host = localHost)

    def received: Trig              = Received(this)
    def sender  : Ex[SocketAddress] = Sender  (this)
    def message : Ex[OscMessage]    = Message (this)

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): OscUdpNode.Repr[T] = {
      val localPortV = localPort.expand[T].value
      val localHostV = localHost.expand[T].value
      import ctx.{cursor, targets}
      new Expanded[T](this, localPortV, localHostV).initExpanded()
    }
  }

  trait Repr[T <: Txn[T]] extends IControl[T] {
    def message(implicit tx: T): osc.Message
    def sender (implicit tx: T): PeerSocketAddress

    def send(target: SocketAddress, p: OscPacket)(implicit tx: T): Unit

    def received: IChangeEvent[T, (osc.Message, PeerSocketAddress)]
  }
}
trait OscUdpNode extends OscNode {
  type Repr[T <: Txn[T]] = OscUdpNode.Repr[T]

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