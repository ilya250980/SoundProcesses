/*
 *  Oscilloscope.scala
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

package de.sciss.proc.gui

import de.sciss.lucre.Disposable
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.swing.LucreSwing._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.{AudioBus, Buffer, Bus, ControlBus, Group, RT, Server, Synth}
import de.sciss.synth.swing.j.AbstractScopePanel
import de.sciss.synth.swing.j.JScopeView.Config
import de.sciss.synth.{AddAction, SynthGraph, addToTail, AudioBus => SAudioBus, Bus => SBus, ControlBus => SControlBus}

import scala.concurrent.stm.{Ref, atomic}

object Oscilloscope {
  def apply(server: Server, style: Int = 0, bufSize: Int = 0, xZoom: Double = 1.0, yZoom: Double = 1.0)
           (implicit tx: RT): Oscilloscope = {
    val res = new Impl(server)
    deferTx {
      res.initGUI(style = style, bufSize = bufSize, xZoom = xZoom, yZoom = yZoom)
    }
    res
  }

  private final class PanelImpl(scope: Impl) extends AbstractScopePanel {
    protected def mkBusSynth(_bus: SBus): Unit = {
      atomic { itx =>
        implicit val tx: RT = RT.wrap(itx)
        scope.mkBusSynthTx(_bus)
      }
    }

    override def mkSynthGraph(_bus: SBus): Unit =
      super.mkSynthGraph(_bus)
  }

  private final class Impl(server: Server) extends Oscilloscope
    with ComponentHolder[PanelImpl]
    with AudioBus   .User
    with ControlBus .User {

    private[this] val busRef  = Ref.make[Bus]()
    private[this] val tgtRef  = Ref.make[Group]()
    private[this] val aaRef   = Ref[AddAction](addToTail)
    private[this] val synRef  = Ref(Option.empty[Synth])
    private[this] var panel: PanelImpl = _

    def initGUI(style: Int, bufSize: Int, xZoom: Double, yZoom: Double): Unit = {
      requireEDT()
      val p         = new PanelImpl(this)
      p.style       = style
      p.bufferSize  = bufSize
      p.xZoom       = xZoom.toFloat
      p.yZoom       = yZoom.toFloat
      panel         = p
      component     = panel
    }

    def target(implicit tx: RT): Group = {
      val _bus    = bus
      val _target = tgtRef()
      if (_target != null || _bus == null) _target else _bus.server.rootNode
    }

    def target_=(value: Group)(implicit tx: RT): Unit = {
      //    val old = target
      tgtRef() = value
      //    if (value != old) {
      //    }
    }

    def addAction(implicit tx: RT): AddAction = aaRef()

    def addAction_=(value: AddAction)(implicit tx: RT): Unit =
      aaRef() = value

    def bus(implicit tx: RT): Bus = busRef()

    def bus_=(b: Bus)(implicit tx: RT): Unit = {
      val old = busRef.swap(b)
      if (old != null) old match {
        case ab: AudioBus   => ab.removeReader(this)
        case cb: ControlBus => cb.removeReader(this)
      }
      if (b != null) {
        require (b.server == server)
        b match {
          case ab: AudioBus   => ab.addReader(this)
          case cb: ControlBus => cb.addReader(this)
        }
      }
    }

    def start()(implicit tx: RT): Unit =
      deferTx {
        component.start()
      }

    def stop()(implicit tx: RT): Unit =
      deferTx {
        component.stop()
      }

    def dispose()(implicit tx: RT): Unit = {
      bus = null
      synRef.swap(None).foreach(_.free())
      deferTx {
        component.dispose()
      }
    }

    def busChanged(peer: SAudioBus, isDummy: Boolean)(implicit tx: RT): Unit = if (!isDummy) {
      deferTx {
        component.bus = peer
      }
    }

    def busChanged(peer: SControlBus)(implicit tx: RT): Unit = {
      deferTx {
        component.bus = peer
      }
    }

    def mkBusSynthTx(_bus: SBus)(implicit tx: RT): Unit = {
      val numChannels = _bus.numChannels
      val oldSyn      = synRef()

      if (numChannels > 0) {
        val gr = SynthGraph {
          panel.mkSynthGraph(_bus)
        }
        val useFrames = panel.bufferSize
        val s         = _bus.server
        // this is a bit tricky; we don't know the buffer and node at this point, so we have to copy the object
        val cfg0      = Config.default(s, bufId = -1, useFrames = useFrames, numChannels = numChannels, nodeId = -1)
        val bufFrames = cfg0.bufFrames
        val b         = Buffer(server)(numFrames = bufFrames, numChannels = numChannels)
        val trFreq    = Config.defaultTrigFreq(s)
        val syn       = Synth.play(gr, nameHint = Some("scope"))(target = target,
          args = List("out" -> _bus.index, "buf" -> b.id, "freq" -> trFreq),
          addAction = addAction, dependencies = b :: Nil)
        val cfg       = cfg0.copy(bufId = b.id, nodeId = syn.peer.id)
        syn.onEndTxn { implicit tx => b.dispose() }
        synRef()      = Some(syn)

        deferTx {
          panel.view.config = cfg
        }

      } else {
        deferTx {
          panel.view.config = Config.Empty
        }
      }
      oldSyn.foreach(_.dispose())
    }
  }
}
trait Oscilloscope extends Disposable[RT] {

  def bus(implicit tx: RT): Bus

  def bus_=(b: Bus)(implicit tx: RT): Unit

  def target(implicit tx: RT): Group

  def target_=(value: Group)(implicit tx: RT): Unit

  def addAction(implicit tx: RT): AddAction

  def addAction_=(value: AddAction)(implicit tx: RT): Unit

  def start()(implicit tx: RT): Unit
  def stop ()(implicit tx: RT): Unit

  /** The swing component showing the scope. Must be called on the EDT */
  def component: AbstractScopePanel
}