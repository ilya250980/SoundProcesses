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

package de.sciss.lucre.expr.graph

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.{peer => txPeer}
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.impl.{DummyObservableImpl, ObservableImpl}
import de.sciss.lucre.synth.{AnyTxn, Executor, RT, Server, Txn}
import de.sciss.lucre.{ExprLike, synth, Txn => LTxn}
import de.sciss.proc.AuralSystem.Failed
import de.sciss.proc.Runner.Message
import de.sciss.proc.{ExprContext, SoundProcesses, Universe}
import de.sciss.synth.Client
import de.sciss.{osc, proc}

import scala.concurrent.stm.{Ref, TxnLocal}

object AuralSystem extends ProductReader[Runner] {
  def apply(): Runner = new Impl

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Runner = {
    require (arity == 0 && adj == 0)
    AuralSystem()
  }

  /** Runner argument key. Value should be string */
  final val Program             = "program"
  /** Runner argument key. Value should be integer */
  final val ControlBusChannels  = "control-bus-channels"
  /** Runner argument key. Value should be integer */
  final val AudioBusChannels    = "audio-bus-channels"
  /** Runner argument key. Value should be integer */
  final val OutputBusChannels   = "output-bus-channels"
  /** Runner argument key. Value should be integer */
  final val InputBusChannels    = "input-bus-channels"
  /** Runner argument key. Value should be integer */
  final val BlockSize           = "block-size"
  /** Runner argument key. Value should be integer */
  final val SampleRate          = "sample-rate"
  /** Runner argument key. Value should be integer */
  final val AudioBuffers        = "audio-buffers"
  /** Runner argument key. Value should be integer */
  final val MaxNodes            = "max-nodes"
  /** Runner argument key. Value should be integer */
  final val MaxSynthDefs        = "max-synth-defs"
  /** Runner argument key. Real-time memory in bytes. Value should be integer */
  final val MemorySize          = "memory-size"
  /** Runner argument key. Value should be integer */
  final val WireBuffers         = "wire-buffers"
  /** Runner argument key. Value should be integer */
  final val RandomSeeds         = "random-seeds"
  /** Runner argument key. Value should be boolean */
  final val LoadSynthDefs       = "load-synth-defs"
  /** Runner argument key. Value should be tuple (string, string) */
  final val MachPortName        = "mach-port-name"
  /** Runner argument key. Value should be integer */
  final val Verbosity           = "verbosity"
  /** Runner argument key. Value should be sequence of strings */
  final val PlugInsPaths        = "plug-ins-paths"
  /** Runner argument key. Value should be string */
  final val RestrictedPath      = "restricted-path"
  /** Runner argument key. Value should be string */
  final val Host                = "host"
  /** Runner argument key. Value should be integer */
  final val Port                = "port"
  /** Runner argument key. Value should be string */
  final val Transport           = "transport"
  /** Runner argument key. Value should be string */
  final val InputStreamsEnabled = "input-streams-enabled"
  /** Runner argument key. Value should be string */
  final val OutputStreamsEnabled= "output-streams-enabled"
  /** Runner argument key. Value should be string */
  final val DeviceName          = "device-name"
  /** Runner argument key. Value should be a tuple (string, string) */
  final val DeviceNames         = "device-names"
  /** Runner argument key. Value should be integer */
  final val HardwareBlockSize   = "hardware-block-size"
  /** Runner argument key. Value should be boolean */
  final val ZeroConf            = "zero-conf"
  /** Runner argument key. Value should be integer */
  final val MaxLogins           = "max-logins"
  /** Runner argument key. Value should be string */
  final val SessionPassword     = "session-password"

  /** Runner argument key. Value should be integer */
  final val ClientId            = "client-id"
  /** Runner argument key. Value should be integer */
  final val NodeIdOffset        = "node-id-offset"
  /** Runner argument key. Value should be double */
  final val Latency             = "latency"

  private final class Expanded[T <: Txn[T]](tx0: T)(implicit val universe: Universe[T])
    extends proc.Runner[T] with ObservableImpl[T, proc.Runner.State] {

    private def peer: proc.AuralSystem = universe.auralSystem

    private[this] val txnRef  = TxnLocal[T]()
    private[this] val attrRef = TxnLocal[proc.Runner.Attr[T]](Context.emptyAttr[T])

    private[this] val obs = peer.react { rt => peerState =>
      atomic("AuralSystem state", rt) { implicit tx =>
        peerState match {
          case Failed(ex) =>
            val mTxt  = ex.toString
            val m     = proc.Runner.Message(System.currentTimeMillis(), proc.Runner.Message.Error, mTxt)
            messages.current = m :: Nil

          case _ =>
        }
        fire(peerState.toRunnerState)
      }
    } (tx0)

    private def atomic(name: String, rt: RT)(fun: T => Unit): Unit = {
      val txOld = txnRef.get(rt.peer)
      if (txOld != null) fun(txOld) else rt.afterCommit {
        SoundProcesses.step[T](name) { txNew =>
          txnRef.set(txNew)(txNew.peer)
          fun(txNew)
        }  (universe.cursor)
      }
    }

    override def state(implicit tx: T): proc.Runner.State =
      peer.state.toRunnerState

    override def prepare(attr: proc.Runner.Attr[T])(implicit tx: T): Unit =
      attrRef.set(attr)

    override def run()(implicit tx: T): Unit = {
      txnRef.set(tx)
      val sCfg = Server.Config()
      val cCfg = Client.Config()
      Bounce.applyAudioPreferences(sCfg, cCfg)
      val attr = attrRef.get

      def exprAttr(key: String)(set: Any => Unit): Unit =
        attr.get(key) match {
          case Some(ex: ExprLike[T, _]) => set(ex.value)
          case _ =>
        }

      def stringAttr(key: String)(set: String => Unit): Unit = exprAttr(key) {
        case v: String => set(v)
        case _ =>
      }

      def intAttr(key: String)(set: Int => Unit): Unit = exprAttr(key) {
        case v: Int => set(v)
        case _ =>
      }

      def booleanAttr(key: String)(set: Boolean => Unit): Unit = exprAttr(key) {
        case v: Boolean => set(v)
        case _ =>
      }

      // server config
      stringAttr  (Program              )(sCfg.program_=            )
      stringAttr  (RestrictedPath       )(v => sCfg.restrictedPath        = if (v.isEmpty) None else Some(v))
      stringAttr  (Host                 )(sCfg.host_=               )
      stringAttr  (InputStreamsEnabled  )(v => sCfg.inputStreamsEnabled   = if (v.isEmpty) None else Some(v))
      stringAttr  (OutputStreamsEnabled )(v => sCfg.outputStreamsEnabled  = if (v.isEmpty) None else Some(v))
      stringAttr  (DeviceName           )(v => sCfg.deviceName            = if (v.isEmpty) None else Some(v))
      stringAttr  (SessionPassword      )(v => sCfg.sessionPassword       = if (v.isEmpty) None else Some(v))
      intAttr     (ControlBusChannels   )(sCfg.controlBusChannels_= )
      intAttr     (AudioBusChannels     )(sCfg.audioBusChannels_=   )
      intAttr     (OutputBusChannels    )(sCfg.outputBusChannels_=  )
      intAttr     (InputBusChannels     )(sCfg.inputBusChannels_=   )
      intAttr     (BlockSize            )(sCfg.blockSize_=          )
      intAttr     (SampleRate           )(sCfg.sampleRate_=         )
      intAttr     (AudioBuffers         )(sCfg.audioBuffers_=       )
      intAttr     (MaxNodes             )(sCfg.maxNodes_=           )
      intAttr     (MaxSynthDefs         )(sCfg.maxSynthDefs_=       )
      intAttr     (MemorySize           )(sCfg.memorySize_=         )
      intAttr     (WireBuffers          )(sCfg.wireBuffers_=        )
      intAttr     (RandomSeeds          )(sCfg.randomSeeds_=        )
      intAttr     (Verbosity            )(sCfg.verbosity_=          )
      intAttr     (Port                 )(sCfg.port_=               )
      intAttr     (HardwareBlockSize    )(sCfg.hardwareBlockSize_=  )
      intAttr     (MaxLogins            )(sCfg.maxLogins_=          )
      booleanAttr (LoadSynthDefs        )(sCfg.loadSynthDefs_=      )
      booleanAttr (ZeroConf             )(sCfg.zeroConf_=           )

      exprAttr(MachPortName) {  // tuple (string, string)
        case (a: String, b: String) if a.nonEmpty || b.nonEmpty => sCfg.machPortName = Some((a, b))
        case _ =>
      }
      exprAttr(PlugInsPaths) {  // sequence of strings
        case xs: Seq[_] if xs.forall(_.isInstanceOf[String]) =>
          sCfg.plugInsPaths = xs.asInstanceOf[Seq[String]].toList
        case _ =>
      }

      if (Executor.isJS) {  // change the default when running Scala.js
        sCfg.transport = osc.Browser
      }

      stringAttr(Transport) { v =>
        v.toLowerCase match {
          case "udp"      => sCfg.transport = osc.UDP
          case "tcp"      => sCfg.transport = osc.TCP
          case "browser"  => sCfg.transport = osc.Browser
          case _ =>
        }
      }

      exprAttr(DeviceNames) {  // tuple (string, string)
        case (a: String, b: String) if a.nonEmpty || b.nonEmpty => sCfg.deviceNames = Some((a, b))
        case _ =>
      }

      // client config

      intAttr(ClientId    )(cCfg.clientId_=     )
      intAttr(NodeIdOffset)(cCfg.nodeIdOffset_= )
      exprAttr(Latency) { // double
        case v: Int     => cCfg.latency = v
        case v: Double  => cCfg.latency = v
        case _ =>
      }

      peer.start(sCfg, cCfg)
    }

    override def stop()(implicit tx: T): Unit = {
      txnRef.set(tx)
      peer.stop()
    }

    override def dispose()(implicit tx: T): Unit =
      obs.dispose()

    override def initControl()(implicit tx: T): Unit = ()

    object messages extends proc.Runner.Messages[T] with ObservableImpl[T, List[Message]] {
      private[this] val ref = Ref(List.empty[Message])

      def current(implicit tx: T): List[Message] = ref()

      def current_=(value: List[Message])(implicit tx: T): Unit = {
        val old = ref.swap(value)
        if (value !== old) fire(value)
      }
    }

    object progress extends proc.Runner.Progress[T] with DummyObservableImpl[T] {
      override def current(implicit tx: T): Double = 0.0
    }
  }

  private final case class Impl() extends Runner {

    override def productPrefix: String = "AuralSystem"  // serialization

    override type Repr[T <: LTxn[T]] = proc.Runner[T]

    protected def mkRepr[T <: LTxn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      tx match {
        case stx: synth.Txn[_] =>
          // ugly...
          val tup = (ctx, stx).asInstanceOf[(Context[AnyTxn], AnyTxn)]
          mkControlImpl(tup).asInstanceOf[Repr[T]]

        case _ => throw new Exception("Need a SoundProcesses system")
      }

    private def mkControlImpl[T <: synth.Txn[T]](tup: (Context[T], T)): Repr[T] = {
      implicit val ctx: Context[T]  = tup._1
      implicit val tx : T           = tup._2
      val ec = ExprContext.get
      import ec.universe
      new Expanded[T](tx)
    }
  }
}