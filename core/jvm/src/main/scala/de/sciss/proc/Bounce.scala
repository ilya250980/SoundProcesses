/*
 *  Bounce.scala
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

import java.io.File

import de.sciss.lucre.synth.Server
import de.sciss.lucre.{Obj, Source, Txn, synth}
import de.sciss.proc.impl.{BounceImpl => Impl}
import de.sciss.processor.ProcessorFactory
import de.sciss.span.Span
import de.sciss.synth.Client

import scala.collection.immutable.{Iterable => IIterable}
import scala.language.implicitConversions

object Bounce {
  def apply[T <: synth.Txn[T]]()(implicit universe: Universe[T]): Bounce[T] =
    new Impl[T](universe)

  private type GroupH[T <: Txn[T]] = IIterable[Source[T, Obj[T]]]

  sealed trait ConfigLike[T <: Txn[T]] {
    /** The group to transport through the bounce.
      * This parameter is initially unspecified in the builder, and calling the getter will throw an error.
      * This parameter must be specified before generating a `Config` instance.
      */
    def group: GroupH[T]

    /** The span of the timeline to bounce. */
    def span: Span

    /** Configuration of the offline server.
      * It is crucial to specify the NRT bits, i.e.
      * `nrtInputPath` (if used), `nrtOutputPath`,
      * `nrtHeaderFormat` (defaults to AIFF), `nrtSampleFormat` (defaults to Float32),
      * as well as the number of input and output channels
      * `inputBusChannels` and `outputBusChannels`, and finally
      * the sampling rate `sampleRate`.
      *
      * Typically you will not specify the `nrtCommandPath` (NRT OSC file). For debugging purposes this
      * may be set, otherwise a temporary file is automatically generated.
      */
    def server: Server.ConfigLike

    def client: Client.ConfigLike

    /** An arbitrary function may be provided which is called when the server is initialized (logical time zero).
      * This entry is typically used to set up extra routing synths, main volume, etc.
      */
    def beforePrepare: (T, Server) => Unit
    def beforePlay   : (T, Server) => Unit

    /** Whether to run the server in real-time or offline. */
    def realtime: Boolean

    def actions: IIterable[Scheduler.Entry[T]]
  }
  object Config {
    val NoOp: (Any, Any) => Unit = (_, _) => ()

    def apply[T <: Txn[T]](): ConfigBuilder[T] = new ConfigBuilder

    implicit def build[T <: Txn[T]](b: ConfigBuilder[T]): Config[T] = b.build
  }
  sealed trait Config[T <: Txn[T]] extends ConfigLike[T] {
    def server: Server.Config
    def client: Client.Config
  }
  final class ConfigBuilder[T <: Txn[T]] private[Bounce] () extends ConfigLike[T] {
    private var _group: GroupH[T] = _
    def group: GroupH[T] = {
      if (_group == null) throw new IllegalStateException("A group has not yet been assigned")
      _group
    }
    def group_=(value: GroupH[T]): Unit = _group = value

    /** The default span is from zero to one second. */
    var span  : Span                    = Span(0L, TimeRef.SampleRate.toLong)
    /** The default server configuration is ScalaCollider's
      * default, a block-size of one, no input and one output channel.
      */
    val server: Server.ConfigBuilder = {
      val res = Server.Config()
      // some sensible defaults
      res.blockSize          = 1
      res.inputBusChannels   = 0
      res.outputBusChannels  = 1
      res
    }

    val client: Client.ConfigBuilder = {
      val res = Client.Config()
      res
    }

    var beforePrepare: (T, Server) => Unit  = Config.NoOp
    var beforePlay   : (T, Server) => Unit  = Config.NoOp

    /** The default mode is offline (realtime == `false`) */
    var realtime: Boolean = false

    var actions: IIterable[Scheduler.Entry[T]] = Nil

    def build: Config[T] = ConfigImpl(group = group, span = span, server = server, client = client,
      beforePrepare = beforePrepare, beforePlay = beforePlay, realtime = realtime, actions = actions)
  }

  private final case class ConfigImpl[T <: Txn[T]](group        : GroupH[T],
                                                   span         : Span,
                                                   server       : Server.Config,
                                                   client       : Client.Config,
                                                   beforePrepare: (T, Server) => Unit,
                                                   beforePlay   : (T, Server) => Unit,
                                                   realtime     : Boolean,
                                                   actions      : IIterable[Scheduler.Entry[T]])
    extends Config[T] {

    override def productPrefix = "Config"
  }

  final case class ServerFailed(code: Int) extends Exception {
    override def toString = s"$productPrefix($code)"
  }
}
trait Bounce[T <: Txn[T]] extends ProcessorFactory {
  type Product  = File
  type Config   = Bounce.Config[T]
  type Repr     = Generic
}