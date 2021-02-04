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

package de.sciss.proc
package impl

import de.sciss.equal.Implicits.TripleEquals
import de.sciss.lucre.Disposable
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.synth.Server.Config
import de.sciss.lucre.synth.{Executor, RT, Server}
import de.sciss.osc.Dump
import de.sciss.proc.AuralSystem.{Preparing, Running, Stopped}
import de.sciss.proc.SoundProcesses.{logAural => logA}
import de.sciss.synth.{Client, ServerConnection, Server => SServer}

import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.stm.Ref
import scala.concurrent.stm.TxnExecutor.{defaultAtomic => atomic}

object AuralSystemImpl {
  import AuralSystem.State

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

  // XXX TODO: Lucre should relax constraints on ObservableImpl. issue #41
  private[impl] final class Impl extends AuralSystem with AuralSystemPlatform {
    impl =>

    private[this] val stateRef        = Ref[State](Stopped)
    private[this] val connection      = new AtomicReference(Option.empty[ServerConnection])
    private[this] val serverListener  = new AtomicReference(Option.empty[SServer.Listener])

    // XXX BEGIN DRY:
    type T = RT
    type U = State

    private[this] final class Observation(val fun: T => U => Unit) extends Disposable[T] {
      def dispose()(implicit tx: T): Unit = removeObservation(this)
    }

    private[this] val obsRef = Ref(Vector.empty[Observation])

    protected def shutdown(): Unit = stateRef.single() match {
      case Running(server) =>
        server.peer.quit()
      case Preparing() =>
        connection.getAndSet(None).foreach(_.abort())
      case _ =>
    }

    protected def fire(update: U)(implicit tx: T): Unit = {
      val obs = obsRef()
      obs.foreach(_.fun(tx)(update))
    }

    private[this] def removeObservation(obs: Observation)(implicit tx: T): Unit =
      obsRef.transform(_.filterNot(_ === obs))

    def react(fun: T => U => Unit)(implicit tx: T): Disposable[T] = {
      val obs = new Observation(fun)
      obsRef.transform(_ :+ obs)(tx.peer)
      obs
    }
    // XXX END DRY

    override def state(implicit tx: RT): State = stateRef.get(tx.peer)

    override def reactNow(fun: RT => State => Unit)(implicit tx: RT): Disposable[RT] = {
      val res = react(fun)
      fun(tx)(state)
      res
    }

    private def initConnection(launch: ServerConnection.Listener => ServerConnection): Unit = {
      logA.debug("Booting")
      val con: ServerConnection = launch {
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

      connection.getAndSet(Some(con)).foreach(_.abort())
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
            val launch = mkConnection(config, client, connect = connect)
            atomic { implicit itx =>
              implicit val tx: RT = RT.wrap(itx)
              state = Preparing()
            }
            initConnection(launch)
          }

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

            case _ =>
          }}
          c
          ()
      }
    }
  }
}