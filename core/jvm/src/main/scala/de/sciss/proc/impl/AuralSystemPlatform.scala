/*
 *  AuralSystemPlatform.scala
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

import de.sciss.lucre.synth.{RT, Server}
import de.sciss.synth.{Client, ServerConnection, Server => SServer}

trait AuralSystemPlatform {
  impl: AuralSystemImpl.Impl =>

  private lazy val installShutdown: Unit = Runtime.getRuntime.addShutdownHook(new Thread(() => impl.shutdown()))

  protected def mkConnection(config: Server.Config, client: Client.Config,
                             connect: Boolean): ServerConnection.Listener => ServerConnection = {
//    tx.afterCommit {
      installShutdown
//    }
    if (connect) {
      SServer.connect("SoundProcesses", config, client)
    } else {
      SServer.boot   ("SoundProcesses", config, client)
    }
  }
}
