/*
 *  SensorSystem.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.osc
import de.sciss.lucre.stm.TxnLike
import de.sciss.osc.{UDP, Channel}
import impl.{SensorSystemImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object SensorSystem {
  final val defaultPort     = 0x5350        // "SP"
  final val defaultCommand  = "/sensor"

  // ---- config ----

  sealed trait ConfigLike {
    def osc: Channel.Net.ConfigLike
    def command: String
  }
  object Config {
    def apply() = new ConfigBuilder

    implicit def build(b: ConfigBuilder): Config = b.build
  }
  final case class Config(osc: Channel.Net.Config, command: String) extends ConfigLike
  final class ConfigBuilder private[SensorSystem] () extends ConfigLike {
    var osc: Channel.Net.ConfigBuilder = UDP.Config()
    osc.localPort = defaultPort

    var command: String = defaultCommand

    def build = Config(osc.build, command = command)
  }

  // ---- instantiation ----

  def apply(): SensorSystem = Impl()

  def start(config: Config = Config())(implicit tx: TxnLike): SensorSystem = {
    val res = apply()
    res.start(config)
    res
  }

  // ---- client ----

  trait Client {
    def sensorsStarted(c: Server)(implicit tx: TxnLike): Unit
    def sensorsStopped()         (implicit tx: TxnLike): Unit
    def sensorsUpdate(values: Vec[Float])(implicit tx: TxnLike): Unit
  }

  type Server = osc.Receiver.Undirected.Net
}
trait SensorSystem {
  import SensorSystem.{Config, Client, Server}

  var dumpOSC: Boolean

  def start(config: Config = SensorSystem.Config())(implicit tx: TxnLike): Unit

  def stop()(implicit tx: TxnLike): Unit

  def addClient   (c: Client)(implicit tx: TxnLike): Unit
  def removeClient(c: Client)(implicit tx: TxnLike): Unit

  def whenStarted(fun: Server => Unit)(implicit tx: TxnLike): Unit

  def serverOption(implicit tx: TxnLike): Option[Server]
}
