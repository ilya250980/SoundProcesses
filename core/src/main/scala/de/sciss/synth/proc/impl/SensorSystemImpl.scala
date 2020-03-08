/*
 *  SensorSystemImpl.scala
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

package de.sciss.synth.proc.impl

import java.net.SocketAddress

import de.sciss.lucre.stm.{Disposable, TxnLike}
import de.sciss.osc
import de.sciss.synth.proc.{SensorSystem, SoundProcesses}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref
import scala.concurrent.stm.TxnExecutor.{defaultAtomic => atomic}

object SensorSystemImpl {
  import de.sciss.synth.proc.SensorSystem.{Client, Server}

  // var dumpOSC = false

  def apply(): SensorSystem = new Impl

  /* There is a bug in Scala-STM which means
   * that calling atomic from within Txn.afterCommit
   * causes an exception. It seems this has to
   * do with the external decider being set?
   */
  private def afterCommit(code: => Unit)(implicit tx: TxnLike): Unit = tx.afterCommit {
    SoundProcesses.scheduledExecutorService.submit(new Runnable() {
      def run(): Unit = code
    })
  }

  private final class Impl extends SensorSystem {
    impl =>

    private sealed trait State extends Disposable[TxnLike] {
      def serverOption: Option[Server]
      def shutdown(): Unit
    }

    private case object StateStopped extends State {
      def dispose()(implicit tx: TxnLike): Unit = ()
      def serverOption: Option[Server] = None
      def shutdown(): Unit = ()
    }

    private case class StateBooting(config: SensorSystem.Config) extends State {
      //      private lazy val con: osc.ClientConnection = {
      //        val launch: osc.ClientConnection.Listener => osc.ClientConnection = if (connect) {
      //          Sosc.Client.connect("SoundProcesses", config)
      //        } else {
      //          Sosc.Client.boot("SoundProcesses", config)
      //        }
      //
      //        // logA(s"Booting (connect = $connect)")
      //        launch {
      //          case osc.ClientConnection.Aborted =>
      //            state.single.swap(StateStopped)
      //          //            atomic { implicit itx =>
      //          //              implicit val tx = Txn.wrap(itx)
      //          //              state.swap(StateStopped).dispose()
      //          //            }
      //
      //          case osc.ClientConnection.Running(s) =>
      //            if (dumpOSC) s.dumpOSC(Dump.Text)
      //            SoundProcesses.pool.submit(new Runnable() {
      //              def run(): Unit = clientStarted(osc.Client(s))
      //            })
      //        }
      //      }

      def init()(implicit tx: TxnLike): Unit = afterCommit {
        val s = config.osc match {
          case c: osc.UDP.Config =>
            val rcv = osc.UDP.Receiver(c)
            rcv.connect()
            rcv
          // case c: osc.TCP.Config => osc.TCP.Server(c)
        }
        setDumpOSC(s, on = _dumpOSC)

        println(s.asInstanceOf[osc.Channel.Net.ConfigLike].localSocketAddress) // XXX TODO
        clientStarted(s, config.command)
      }

      def dispose()(implicit tx: TxnLike): Unit = ()

      def serverOption: Option[Server] = None
      def shutdown(): Unit = ()
    }

    private final class StateRunning(val server: Server, command: String) extends State {
      def dispose()(implicit tx: TxnLike): Unit = {
        // logA("Stopped client")
        // NodeGraph.removeosc.Client(client)
        clients.get(tx.peer).foreach(_.sensorsStopped())

        afterCommit {
          server.action = osc.Channel.Undirected.Input.Net.NoAction
          //          val obs = listener.single.swap(None)
          //          assert(obs.isDefined)
          //          client.peer.removeListener(obs.get)
          if (server.isOpen()) server.close()
        }
      }

      def shutdown(): Unit = server.close()

      def serverOption: Option[Server] = Some(server)

      // private val listener = Ref(Option.empty[Sosc.Client.Listener])

      private def receive(p: osc.Packet, addr: SocketAddress): Unit =
        p match {
          case osc.Message(`command`, valuesU @ _*) =>
            val values = valuesU.asInstanceOf[Seq[Float]].toIndexedSeq
            atomic { implicit itx =>
              implicit val tx: TxnLike = TxnLike.wrap(itx)
              clients.get(tx.peer).foreach(_.sensorsUpdate(values))
            }
          case _ =>
        }

      def init()(implicit tx: TxnLike): Unit = {
        // logA("Started client")
        // NodeGraph.addosc.Client(client)
        afterCommit {
          server.action = receive
        }
        clients.get(tx.peer).foreach(_.sensorsStarted(server))

        //        afterCommit {
        //          val list = server.peer.addListener {
        //            case Sosc.Client.Offline =>
        //              atomic { implicit itx =>
        //                implicit val tx = TxnLike.wrap(itx)
        //                state.swap(StateStopped).dispose()
        //              }
        //          }
        //          val old = listener.single.swap(Some(list))
        //          assert(old.isEmpty)
        //        }
      }
    }

    override def toString = s"SensorSystem@${hashCode.toHexString}"

    private val clients = Ref(Vec   .empty[Client])
    private val state   = Ref(StateStopped: State)

    private var _dumpOSC = false
    def dumpOSC: Boolean = _dumpOSC
    def dumpOSC_=(value: Boolean): Unit = if (value != _dumpOSC) {
      _dumpOSC = value
      state.single.get.serverOption.foreach { server =>
        setDumpOSC(server, on = value)
      }
    }

    private def setDumpOSC(server: Server, on: Boolean): Unit =
      server.dump(if (on) osc.Dump.Text else osc.Dump.Off)

    private def clientStarted(rich: Server, command: String): Unit =
      atomic { implicit itx =>
        implicit val tx: TxnLike = TxnLike.wrap(itx)
        clientStartedTx(rich, command)
      }

    private def clientStartedTx(server: Server, command: String)(implicit tx: TxnLike): Unit = {
      val running = new StateRunning(server, command)
      state.swap(running)(tx.peer) // .dispose()
      running.init()
    }

    def start(config: SensorSystem.Config)(implicit tx: TxnLike): Unit = state.get(tx.peer) match {
      case StateStopped =>
        installShutdown
        val booting = StateBooting(config)
        state.swap(booting)(tx.peer) // .dispose()
        booting.init()

      case _ =>
    }

    private lazy val installShutdown: Unit = Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      def run(): Unit = impl.shutdown()
    }))

    private def shutdown(): Unit = state.single().shutdown()

    def stop()(implicit tx: TxnLike): Unit =
      state.swap(StateStopped)(tx.peer).dispose()

    def addClient(c: Client)(implicit tx: TxnLike): Unit =
      clients.transform(_ :+ c)(tx.peer)

    def serverOption(implicit tx: TxnLike): Option[Server] = state.get(tx.peer).serverOption

    def removeClient(c: Client)(implicit tx: TxnLike): Unit =
      clients.transform { _.filterNot(_ == c) } (tx.peer)

    def whenStarted(fun: Server => Unit)(implicit tx: TxnLike): Unit = {
      state.get(tx.peer) match {
        case r: StateRunning => fun(r.server)
        case _ =>
          val c: Client = new Client {
            def sensorsStarted(s: Server)(implicit tx: TxnLike): Unit = {
              removeClient(this)
              fun(s)
            }

            def sensorsStopped()(implicit tx: TxnLike): Unit = ()

            def sensorsUpdate(values: Vec[Float])(implicit tx: TxnLike): Unit = ()
          }
          addClient(c)
      }
    }
  }
}