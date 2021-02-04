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

import de.sciss.lucre.synth.Server
import de.sciss.synth.{Client, ServerConnection, Server => SServer}
import de.sciss.proc.SoundProcesses.{logAural => logA}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

trait AuralSystemPlatform {
  impl: AuralSystemImpl.Impl =>

  private def boot(config: Server.Config /*, client: Client.Config*/): Unit = {
    val Module                = js.Dynamic.global.Module
    val sArgs: List[String]   = config.toRealtimeArgs.tail  // note 'head' is the command to run (`scsynth`)
    logA.debug(sArgs.mkString(s"scsynth.wasm callMain with args: ", " ", ""))
    val callMain              = Module.callMain.asInstanceOf[js.Function1[js.Array[String], Unit]]
    callMain(sArgs.toJSArray)
  }

  protected def mkConnection(config: Server.Config, client: Client.Config,
                             connect: Boolean): ServerConnection.Listener => ServerConnection = {
    if (!connect) {
      boot(config)
    }
    SServer.connect("SoundProcesses", config, client)
  }
}
