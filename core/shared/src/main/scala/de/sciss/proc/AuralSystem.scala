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

import de.sciss.lucre.synth.{RT, Server}
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

  trait Client {
    def auralStarted(s: Server)(implicit tx: RT): Unit
    def auralStopped()         (implicit tx: RT): Unit
  }
}
/** An `AuralSystem` is the logical representation of a sound synthesis server, whether running or not.
  * To use an aural system, a client connects via `addClient`. The client will be notified when the
   * server is up and running.
  */
trait AuralSystem {
  import AuralSystem.Client

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

  /** Adds a client to the system. It is safe to call this method both inside and
    * outside of a transaction. If called inside a transaction, this is transaction
    * safe (no duplicate registration if the transaction is retried).
    *
    * @param  c the client to register. If the server is already running, the client
    *           will _not_ be immediately notified.
    */
  def addClient(c: Client)(implicit tx: RT): Unit

  /** Same as `addClient`, but additionally calls `auralStarted` if the server is already running. */
  def addClientNow(c: Client)(implicit tx: RT): Unit

  /** Removes a client to the system. It is safe to call this method both inside and
    * outside of a transaction. If called inside a transaction, this is transaction
    * safe.
    *
    * @param  c the client to unregister. It is allowed to call this method even if
    *           the client was already unregistered.
    */
  def removeClient(c: Client)(implicit tx: RT): Unit

  /** Registers a callback to be invoked when the server has been booted.
    * If the server is already running, this has no effect. This method is transaction safe.
    *
    * The function is always execution _outside_ of a transaction.
    */
  def whenStarted(fun: Server => Unit)(implicit tx: RT): Unit

  def serverOption(implicit tx: RT): Option[Server]
}
