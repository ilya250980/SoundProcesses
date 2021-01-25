/*
 *  AuralSystemImpl.scala
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

package de.sciss.proc.impl

import de.sciss.equal.Implicits.TripleEquals
import de.sciss.lucre.Disposable
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.synth.Server.Config
import de.sciss.lucre.synth.{Executor, RT, Server}
import de.sciss.osc.Dump
import de.sciss.proc.AuralSystem
import de.sciss.proc.AuralSystem.{Preparing, Running, Stopped}
import de.sciss.proc.SoundProcesses.{logAural => logA}
import de.sciss.synth.{Client, ServerConnection, Server => SServer}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.stm.Ref
import scala.concurrent.stm.TxnExecutor.{defaultAtomic => atomic}

object AuralSystemImpl {
  import de.sciss.proc.AuralSystem.State

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

  // XXX TODO: Lucre should relax constraints on ObservableImpl
  private final class Impl extends AuralSystem /*with ObservableImpl[RT, State]*/ {
    impl =>

    private[this] val stateRef        = Ref[State](Stopped)
    private[this] val connection      = new AtomicReference(Option.empty[ServerConnection])
    private[this] val serverListener  = new AtomicReference(Option.empty[SServer.Listener])

    override def state(implicit tx: RT): State = stateRef.get(tx.peer)

    private[this] final class Observation(val fun: RT => State => Unit) extends Disposable[RT] {
      def dispose()(implicit tx: RT): Unit = removeObservation(this)
    }

    private[this] val obsRef = Ref(Vector.empty[Observation])

    private def fire(update: State)(implicit tx: RT): Unit = {
      val obs = obsRef()(tx.peer)
      obs.foreach(_.fun(tx)(update))
    }

    private[this] def removeObservation(obs: Observation)(implicit tx: RT): Unit =
      obsRef.transform(_.filterNot(_ === obs))(tx.peer)

    override def react(fun: RT => State => Unit)(implicit tx: RT): Disposable[RT] = {
      val obs = new Observation(fun)
      obsRef.transform(_ :+ obs)(tx.peer)
      obs
    }

    override def reactNow(fun: RT => State => Unit)(implicit tx: RT): Disposable[RT] = {
      val res = react(fun)
      fun(tx)(state)
      res
    }

    private def initConnection(launch: ServerConnection.Listener => ServerConnection)(implicit tx: RT): Unit = {
      lazy val con: ServerConnection = {
        logA.debug("Booting")
        launch {
          case ServerConnection.Aborted =>
            connection.set(None)
            serverStopped()

          case ServerConnection.Running(s) =>
            connection.set(None)
            if (dumpOSC) s.dumpOSC(Dump.Text)
            Executor.defer {
              serverStarted(Server(s))
            }
        }
      }

      afterCommit {
        val c = con
        connection.getAndSet(Some(c)).foreach(_.abort())
      }
    }

    override def toString = s"AuralSystem@${hashCode.toHexString}"

    def offline(server: Server.Offline)(implicit tx: RT): Unit = serverStartedTx(server)

    private def serverStarted(rich: Server): Unit =
      atomic { implicit itx =>
        implicit val tx: RT = RT.wrap(itx)
        serverStartedTx(rich)
      }

    private def serverStopped(): Unit =
      atomic { implicit itx =>
        implicit val tx: RT = RT.wrap(itx)
        state = Stopped
      }

    // put this into a separate method because `atomic` will otherwise
    // pick up an obsolete transaction in implicit scope
    private def addServerListener(server: Server): Unit = {
      val l = server.peer.addListener {
        case SServer.Offline => serverStopped()
      }
      val old = serverListener.getAndSet(Some(l))
      assert(old.isEmpty)
    }

    private def state_=(value: State)(implicit tx: RT): Unit = {
      val old = stateRef.swap(value)(tx.peer)
      if (value != old) {
        (old, value) match {
          case (Running(server), _) =>
            logA.debug("Stopped server")
            afterCommit {
              val obs = serverListener.getAndSet(None)
              assert(obs.isDefined)
              server.peer.removeListener(obs.get)
              if (server.peer.isRunning) server.peer.quit()
            }

          case (Preparing(), Stopped) =>
            tx.afterCommit {
              // could have been user issued `stop`, thus cancel connection if it still exists
              connection.getAndSet(None).foreach(_.abort())
            }

          case _ =>
        }
        fire(value)
      }
    }

    private def serverStartedTx(server: Server)(implicit tx: RT): Unit = {
      logA.debug("Started server")
      state = Running(server)
      afterCommit(addServerListener(server))
    }

    def start(config: Server.Config, client: Client.Config, connect: Boolean)(implicit tx: RT): Unit =
      state match {
        case Stopped =>
          tx.afterCommit {
            installShutdown
          }
          val launch: ServerConnection.Listener => ServerConnection = if (connect) {
            SServer.connect("SoundProcesses", config, client)
          } else {
            SServer.boot   ("SoundProcesses", config, client)
          }
          state = Preparing()
          initConnection(launch)

        case _ =>
      }

    override def connect(config: Config, client: Client.Config)(implicit tx: RT): Unit =
      state match {
        case Stopped =>
          val launch: ServerConnection.Listener => ServerConnection =
            SServer.connect("SoundProcesses", config, client)

          state = Preparing()
          initConnection(launch)

        case _ =>
      }

    private lazy val installShutdown: Unit = Runtime.getRuntime.addShutdownHook(new Thread(() => impl.shutdown()))

    private def shutdown(): Unit = stateRef.single() match {
      case Running(server) =>
        server.peer.quit()
      case Preparing() =>
        connection.getAndSet(None).foreach(_.abort())
      case _ =>
    }

    def stop()(implicit tx: RT): Unit =
      state = Stopped

    def serverOption(implicit tx: RT): Option[Server] = state match {
      case Running(s) => Some(s)
      case _          => None
    }

    def whenStarted(fun: Server => Unit)(implicit tx: RT): Unit = {
      state match {
        case Running(server) => tx.afterCommit(fun(server))
        case _ =>
          lazy val c: Disposable[RT] = react { implicit tx => {
            case Running(server) =>
              c.dispose()
              tx.afterCommit(fun(server))
          }}
          c
          ()
      }
    }
  }
}