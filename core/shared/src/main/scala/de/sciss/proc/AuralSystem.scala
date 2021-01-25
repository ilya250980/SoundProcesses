/*
 *  AuralSystem.scala
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

import de.sciss.lucre.{Disposable, Observable}
import de.sciss.lucre.synth.{RT, Server}
import de.sciss.proc.AuralSystem.State
import de.sciss.synth.Client
import de.sciss.proc.impl.{AuralSystemImpl => Impl}

object AuralSystem {
  def apply(global: Boolean = false): AuralSystem = Impl(global = global)

  def start(config: Server.Config = Server.Config(), client: Client.Config = Client.Config(),
            connect: Boolean = false)(implicit tx: RT): AuralSystem = {
    val res = apply()
    res.start(config, client, connect = connect)
    res
  }

  /** Creates an offline-server based aural system.
    * It is important that the `AuralSystem` is eventually
    * disposed again, calling the `stop` method.
    */
  def offline(server: Server.Offline)(implicit tx: RT): AuralSystem = {
    val res = apply()
    res.offline(server)
    res
  }

  sealed trait State {
    /** Stopped for failed */
    def idle   : Boolean
    /** Preparing or running */
    def busy   : Boolean = !idle
    def failed : Boolean
    def stopped: Boolean
    def running: Boolean

    // it's unfortunate that it doesn't have a playload (server) and thus we need to convert
    def toRunnerState: Runner.State
  }
  case object Stopped extends State {
    final val idle    = true
    final val failed  = false
    final val stopped = true
    final val running = false

    def toRunnerState: Runner.State = Runner.Stopped
  }
  case class Preparing() extends State {  // could add server connection later
    final val idle    = false
    final val failed  = false
    final val stopped = false
    final val running = false

    def toRunnerState: Runner.State = Runner.Preparing
  }
  case class Running(server: Server) extends State {
    final val idle    = false
    final val failed  = false
    final val stopped = false
    final val running = true

    def toRunnerState: Runner.State = Runner.Running
  }
  final case class Failed(ex: Throwable) extends State {
    final val idle    = true
    final val failed  = true
    final val stopped = false
    final val running = false

    def toRunnerState: Runner.State = Runner.Failed(ex)
  }
}
/** An `AuralSystem` is the logical representation of a sound synthesis server, whether running or not.
  * To use an aural system, a client connects via `addClient`. The client will be notified when the
   * server is up and running.
  */
trait AuralSystem extends Observable[RT, State] {

  def state(implicit tx: RT): State

  def reactNow(fun: RT => State => Unit)(implicit tx: RT): Disposable[RT]

  /** Boots the server, or connects to an existing one.
    * This method must be called from within a transaction.
    *
    * @param  connect if `true`, tries to connect to a running server; if `false`,
    *                 starts a new scsynth process.
    */
  def start(config: Server.Config = Server.Config(), client: Client.Config = Client.Config(),
            connect: Boolean = false)(implicit tx: RT): Unit

  /** Connects to a running server. */
  def connect(config: Server.Config = Server.Config(), client: Client.Config = Client.Config())
             (implicit tx: RT): Unit

  private[proc] def offline(server: Server.Offline)(implicit tx: RT): Unit

  /** Quits the server. This method must not be called from within a transaction. */
  def stop()(implicit tx: RT): Unit

  /** Registers a callback to be invoked when the server has been booted.
    * If the server is already running, this has no effect. This method is transaction safe.
    *
    * The function is always execution _outside_ of a transaction.
    */
  def whenStarted(fun: Server => Unit)(implicit tx: RT): Unit

  def serverOption(implicit tx: RT): Option[Server]
}
