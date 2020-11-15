/*
 *  AuralSystemImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.Disposable
import de.sciss.lucre.synth.{Executor, RT, Server}
import de.sciss.osc.Dump
import de.sciss.proc.AuralSystem
import de.sciss.synth.{Client, ServerConnection, Server => SServer}
import de.sciss.proc.SoundProcesses.{logAural => logA}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.Ref
import scala.concurrent.stm.TxnExecutor.{defaultAtomic => atomic}

object AuralSystemImpl {
  import de.sciss.proc.AuralSystem.Client

  var dumpOSC = false

  private val sync = new AnyRef
  private var globalInstance: AuralSystem = _

  def apply(global: Boolean): AuralSystem =
    if (global) {
      sync.synchronized(
        if (globalInstance != null) globalInstance
        else {
          val res = new Impl
          globalInstance = res
          res
        }
      )
    } else {
      new Impl
    }


  /* There is a bug in Scala-STM which means
   * that calling atomic from within Txn.afterCommit
   * causes an exception. It seems this has to
   * do with the external decider being set?
   *
   * TODO: review
   */
  private def afterCommit(code: => Unit)(implicit tx: RT): Unit = tx.afterCommit {
    val exec = Executor
    // note: `isShutdown` is true during VM shutdown. In that case
    // calling `submit` would throw an exception.
    if (exec.isShutdown) code else exec.defer(code)
  }

  private final class Impl extends AuralSystem {
    impl =>

    private sealed trait State extends Disposable[RT] {
      def serverOption: Option[Server]
      def shutdown(): Unit
    }

    private case object StateStopped extends State {
      def dispose()(implicit tx: RT): Unit = ()
      def serverOption: Option[Server] = None
      def shutdown(): Unit = ()
    }

    private case class StateBooting(config: Server.Config, client: Client.Config, connect: Boolean) extends State {
      private lazy val con: ServerConnection = {
        val launch: ServerConnection.Listener => ServerConnection = if (connect) {
          SServer.connect("SoundProcesses", config, client)
        } else {
          SServer.boot   ("SoundProcesses", config, client)
        }

        logA.debug(s"Booting (connect = $connect)")
        launch {
          case ServerConnection.Aborted =>
            state.single.set(StateStopped)
            //            atomic { implicit itx =>
            //              implicit val tx = Txn.wrap(itx)
            //              state.swap(StateStopped).dispose()
            //            }

          case ServerConnection.Running(s) =>
            if (dumpOSC) s.dumpOSC(Dump.Text)
            Executor.defer { serverStarted(Server(s)) }
        }
      }

      def init()(implicit tx: RT): Unit = afterCommit {
        con
        ()
      }

      def dispose()(implicit tx: RT): Unit = afterCommit {
        logA.debug("Aborting boot")
        con.abort()
      }

      def serverOption: Option[Server] = None
      def shutdown(): Unit = con.abort()
    }

    private case class StateRunning(server: Server) extends State {
      def dispose()(implicit tx: RT): Unit = {
        logA.debug("Stopped server")
        // NodeGraph.removeServer(server)
        clients.get(tx.peer).foreach(_.auralStopped())

        afterCommit {
          val obs = listener.single.swap(None)
          assert(obs.isDefined)
          server.peer.removeListener(obs.get)
          if (server.peer.isRunning) server.peer.quit()
        }
      }

      def shutdown(): Unit = server.peer.quit()

      def serverOption: Option[Server] = Some(server)

      private val listener = Ref(Option.empty[SServer.Listener])

      // put this into a separate method because `atomic` will otherwise
      // pick up an obsolete transaction in implicit scope
      private def ac(): Unit = {
        val list = server.peer.addListener {
          case SServer.Offline =>
            atomic { implicit itx =>
              implicit val tx: RT = RT.wrap(itx)
              state.swap(StateStopped).dispose()
            }
        }
        val old = listener.single.swap(Some(list))
        assert(old.isEmpty)
      }

      def init()(implicit tx: RT): Unit = {
        logA.debug("Started server")
        // NodeGraph.addServer(server)
        clients.get(tx.peer).foreach(_.auralStarted(server))

        afterCommit(ac())
      }
    }

    override def toString = s"AuralSystem@${hashCode.toHexString}"

    private val clients = Ref(Vec   .empty[Client])
    private val state   = Ref(StateStopped: State)

    def offline(server: Server.Offline)(implicit tx: RT): Unit = serverStartedTx(server)

    private def serverStarted(rich: Server): Unit =
      atomic { implicit itx =>
        implicit val tx: RT = RT.wrap(itx)
        serverStartedTx(rich)
      }

    private def serverStartedTx(rich: Server)(implicit tx: RT): Unit = {
      val running = StateRunning(rich)
      state.swap(running)(tx.peer) // .dispose()
      running.init()
    }

    def start(config: Server.Config, client: Client.Config, connect: Boolean)(implicit tx: RT): Unit =
      state.get(tx.peer) match {
        case StateStopped =>
          installShutdown
          val booting = StateBooting(config, client, connect = connect)
          state.swap(booting)(tx.peer) // .dispose()
          booting.init()

        case _ =>
      }

    private lazy val installShutdown: Unit = Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      def run(): Unit = impl.shutdown()
    }))

    private def shutdown(): Unit = state.single().shutdown()

    def stop()(implicit tx: RT): Unit =
      state.swap(StateStopped)(tx.peer).dispose()

    def addClient(c: Client)(implicit tx: RT): Unit = {
      clients.transform(_ :+ c)(tx.peer)
      // serverOption.foreach(c.auralStarted)
    }

    def addClientNow(c: Client)(implicit tx: RT): Unit = {
      addClient(c)
      serverOption.foreach(c.auralStarted)
    }

    def serverOption(implicit tx: RT): Option[Server] = state.get(tx.peer).serverOption

    def removeClient(c: Client)(implicit tx: RT): Unit =
      clients.transform { _.filterNot(_ == c) } (tx.peer)

    def whenStarted(fun: Server => Unit)(implicit tx: RT): Unit = {
      state.get(tx.peer) match {
        case StateRunning(server) => tx.afterCommit(fun(server))
        case _ =>
          val c: Client = new Client {
            def auralStarted(server: Server)(implicit tx: RT): Unit = {
              removeClient(this)
              tx.afterCommit(fun(server))
            }

            def auralStopped()(implicit tx: RT): Unit = ()
          }
          addClient(c)
      }
    }
  }
}