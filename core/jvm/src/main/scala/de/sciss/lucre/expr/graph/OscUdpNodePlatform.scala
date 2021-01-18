/*
 *  OscUdpNodePlatform.scala
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
import de.sciss.lucre.{Cursor, IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.graph.OscUdpNode.Repr
import de.sciss.lucre.expr.impl.IControlImpl
import de.sciss.lucre.impl.{IChangeEventImpl, IChangeGeneratorEvent}
import de.sciss.model.Change
import de.sciss.osc
import de.sciss.proc.SoundProcesses

import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.stm.Ref
import scala.util.control.NonFatal

trait OscUdpNodePlatform {
  type PeerSocketAddress = InetSocketAddress

  protected final class SenderExpanded[T <: Txn[T]](peer: Repr[T], tx0: T)
                                                 (implicit protected val targets: ITargets[T])
    extends IExpr[T, SocketAddress] with IChangeEventImpl[T, SocketAddress] {

    peer.received.--->(changed)(tx0)

    def value(implicit tx: T): SocketAddress = {
      val s = peer.sender
      SocketAddress(s.getHostString, s.getPort)
    }

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): SocketAddress = {
      val (_, addr) = pull.applyChange(peer.received)
      val name      = addr.getHostString
      val port      = addr.getPort
      SocketAddress(name, port)
    }

    def dispose()(implicit tx: T): Unit =
      peer.received.-/->(changed)

    def changed: IChangeEvent[T, SocketAddress] = this
  }

  protected final class Expanded[T <: Txn[T]](protected val peer: OscUdpNode, localPort: Int, localHost: String)
                                           (implicit protected val targets: ITargets[T],
                                            protected val cursor: Cursor[T])
    extends Repr[T] with IControlImpl[T] with IChangeGeneratorEvent[T, (osc.Message, PeerSocketAddress)] {

    private[this] val dummySocket = InetSocketAddress.createUnresolved("invalid", 0)

    private[this] val lastRcvRef = Ref[(osc.Message, PeerSocketAddress)](
      (osc.Message(""), dummySocket))

    //    private[this] val lastTrnsRef = Ref[(SocketAddress, PeerSocketAddress)]((SocketAddress("invalid", 0), dummySocket))
    private[this] val lastTrnsRef = new AtomicReference[(SocketAddress, PeerSocketAddress)]((SocketAddress("invalid", 0), dummySocket))

    def message (implicit tx: T): osc.Message        = lastRcvRef()._1
    def sender  (implicit tx: T): PeerSocketAddress  = lastRcvRef()._2

    def received: IChangeEvent[T, (osc.Message, PeerSocketAddress)] = this

    private[lucre] def pullChange(pull: IPull[T])
                                 (implicit tx: T, phase: IPull.Phase): (osc.Message, PeerSocketAddress) =
      pull.resolveChange

    private[this] var addrHaveWarned = false

    private[this] val receiverFun: osc.Receiver.Undirected.Action = { (p, addr) =>
      addr match {
        case iAddr: PeerSocketAddress =>
          SoundProcesses.step[T]("OscUdpNode.receive") { implicit tx =>
            def fire1(m: osc.Message): Unit = {
              val tupNow    = (m, iAddr)
              val tupBefore = lastRcvRef.swap(tupNow)
              fire(Change(tupBefore, tupNow))
            }

            def fireAll(pp: osc.Packet): Unit =
              pp match {
                case m: osc.Message => fire1(m)
                case b: osc.Bundle  =>
                  b.packets.foreach(fireAll)
              }

            fireAll(p)
          }

        case _ =>
          if (!addrHaveWarned) {
            addrHaveWarned = true
            println("OscNode - sender's address is not PeerSocketAddress. Dropping packet.")
          }
      }
    }

    @volatile
    private[this] var transmitter : osc.UDP.Transmitter .Undirected = _
    private[this] var receiver    : osc.UDP.Receiver    .Undirected = _

    def send(target: SocketAddress, p: OscPacket)(implicit tx: T): Unit =
      tx.afterCommit {
        sendNow(target, p)
      }

    private def sendWith(target: PeerSocketAddress, p: OscPacket): Unit = {
      val p1: osc.Packet = p match {
        case m: OscMessage =>
          osc.Message(m.name, m.args: _*) // XXX TODO --- adjust args to codec

        //        case _: OscBundle => ...
      }
      tryThunk(s"send to $target from") {
        val t = transmitter
        if (t != null) t.send(p1, target)
      }
    }

    private def sendNow(target: SocketAddress, p: OscPacket): Unit = {
      val (lastTarget, lastSck) = lastTrnsRef.get()
      if (lastTarget == target) sendWith(lastSck, p)
      else tryThunk(s"resolve target $target in") {
        val res = new PeerSocketAddress(target.host, target.port)
        lastTrnsRef.set((target, res))
        sendWith(res, p)
      }
    }

    override def dispose()(implicit tx: T): Unit = {
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

    private def tryThunk(op: => String)(thunk: => Unit): Unit =
      try {
        thunk
      } catch {
        case NonFatal(ex) =>
          println(s"Could not $op OscUdpNode")
          if (ex.isInstanceOf[java.nio.channels.UnresolvedAddressException]) {
            Console.err.println(ex.getClass.getName)
          } else {
            ex.printStackTrace()
          }
      }

    private[this] var codec: osc.PacketCodec = _

    override def initControl()(implicit tx: T): Unit = {
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

    //    private def dump(implicit tx: T): osc.Dump = dumpRef()

    private def dump_=(value: osc.Dump)(implicit tx: T): Unit =
      if (dumpRef.swap(value) != value) tx.afterCommit {
        val t = transmitter
        if (t != null) t.dump(value)
        val r = receiver
        if (r != null) r.dump(value)
      }

    private[this] val dumpRef = Ref[osc.Dump](osc.Dump.Off)

    private def dumpModeSafe(id: Int): osc.Dump =
      osc.Dump(math.max(0, math.min(osc.Dump.Both.id, id)))

    def initExpanded()(implicit tx: T, ctx: Context[T]): this.type = {
      val codecS: String = ctx.getProperty[Ex[String]](peer, OscNode.keyCodec)
        .fold(OscNode.defaultCodec)(_.expand[T].value)

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

}
