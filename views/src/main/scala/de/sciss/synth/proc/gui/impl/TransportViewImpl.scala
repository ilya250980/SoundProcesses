/*
 *  TransportViewImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package gui
package impl

import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.event.{ChangeEvent, ChangeListener}

import de.sciss.audiowidgets.{TimelineModel, Transport => GUITransport}
import de.sciss.desktop.Implicits._
import de.sciss.desktop.{FocusType, KeyStrokes}
import de.sciss.lucre.stm.{Cursor, Disposable}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.span.Span

import scala.concurrent.stm.Ref
import scala.swing.Swing.HStrut
import scala.swing.event.Key
import scala.swing.{Action, BoxPanel, Component, Orientation, Swing}

object TransportViewImpl {
  def apply[S <: Sys[S]](transport: Transport[S] /* .Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */,
                         model: TimelineModel, hasMillis: Boolean, hasLoop: Boolean, hasShortcuts: Boolean)
                        (implicit tx: S#Tx, cursor: Cursor[S]): TransportView[S] = {
    val view    = new Impl(transport, model)
    val srk     = 1000 / TimeRef.SampleRate // transport.sampleRate

    view.observer = transport.react { implicit tx => {
      case Transport.Play(_, time) => view.startedPlaying(time)
      case Transport.Stop(_, time) => view.stoppedPlaying(time)
      case Transport.Seek(_, time, p) =>
        if (p) view.startedPlaying(time) else view.stoppedPlaying(time)
      case _ =>
    }}

    val initPlaying = transport.isPlaying // .playing.value
    val initMillis = (transport.position * srk).toLong
    deferTx {
      view.guiInit(initPlaying, initMillis, hasMillis = hasMillis, hasLoop = hasLoop, hasShortcuts = hasShortcuts)
    }
    view
  }

  private final class Impl[S <: Sys[S]](val transport: Transport[S] /* .Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */,
                                        val timelineModel: TimelineModel)
                                       (implicit protected val cursor: Cursor[S])
    extends TransportView[S] with ComponentHolder[Component] with CursorHolder[S] {

    private val modOpt  = timelineModel.modifiableOption

    var observer: Disposable[S#Tx] = _

    // private var millisVar: Long = 0L
    private var playTimer: javax.swing.Timer = _
    private var cueTimer : javax.swing.Timer = _
    // private var playingVar   = false
    private var cueDirection = 1

    private var timerFrame  = 0L
    private var timerSys    = 0L
    private val srm         = 0.001 * TimeRef.SampleRate // transport.sampleRate

    private var transportStrip: Component with GUITransport.ButtonStrip = _

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      cancelLoop()
      deferTx {
        playTimer.stop()
        cueTimer .stop()
      }
    }

    private def cancelLoop()(implicit tx: S#Tx): Unit =
      transport.scheduler.cancel(loopToken.swap(-1)(tx.peer))

    // ---- transport ----
    import GUITransport.{Play, Stop, GoToBegin, Rewind, FastForward, Loop}

    def startedPlaying(time: Long)(implicit tx: S#Tx): Unit = {
      checkLoop()
      deferTx {
        playTimer.stop()
        cueTimer .stop()
        timerFrame  = time
        timerSys    = System.currentTimeMillis()
        playTimer.start()
        transportStrip.button(Play).foreach(_.selected = true )
        transportStrip.button(Stop).foreach(_.selected = false)
      }
    }

    def stoppedPlaying(time: Long)(implicit tx: S#Tx): Unit = {
      cancelLoop()
      deferTx {
        playTimer.stop()
        // cueTimer .stop()
        modOpt.foreach(_.position = time) // XXX TODO if Cursor follows play-head
        transportStrip.button(Play).foreach(_.selected = false)
        transportStrip.button(Stop).foreach(_.selected = true )
      }
    }

    private def rtz(): Unit = {
      stop()
      modOpt.foreach { mod =>
        val start     = mod.bounds.start
        mod.position  = start
        mod.visible   = Span(start, start + mod.visible.length)
      }
    }

    private def playOrStop(): Unit =
      atomic { implicit tx =>
        if (transport.isPlaying) transport.stop() else {
          transport.seek(timelineModel.position)
          transport.play()
        }
      }

    private def stop(): Unit =
      atomic { implicit tx => transport.stop() }

    private def play(): Unit =
      atomic { implicit tx => playTxn(timelineModel.position) }

    private def playTxn(pos: Long)(implicit tx: S#Tx): Unit = {
      transport.stop()
      transport.seek(pos)
      transport.play()
    }

    //    private def rewind()      = ()
    //    private def fastForward() = ()

    private val loopSpan  = Ref[Span.SpanOrVoid](Span.Void)
    private val loopToken = Ref(-1)

    private def toggleLoop(): Unit = transportStrip.button(Loop).foreach { ggLoop =>
      val wasLooping  = ggLoop.selected
      val sel         = if (wasLooping) Span.Void else timelineModel.selection
      val isLooping   = sel.nonEmpty
      ggLoop.selected = isLooping
      atomic { implicit tx =>
        loopSpan.set(sel)(tx.peer)
        if (transport.isPlaying) {
          cancelLoop()
          checkLoop()
        }
      }
    }

    private def checkLoop()(implicit tx: S#Tx): Unit = {
      val pos       = transport.position
      val loopStop  = loopSpan.get(tx.peer) match { case hs: Span.HasStop => hs.stop; case _ => Long.MinValue }
      if (loopStop > pos) {
        val sched   = transport.scheduler
        val time    = sched.time + (loopStop - pos)
        val token   = sched.schedule(time) { implicit tx => loopEndReached() }
        val old     = loopToken.swap(token)(tx.peer)
        sched.cancel(old)
      }
    }

    private def loopEndReached()(implicit tx: S#Tx): Unit = loopSpan.get(tx.peer) match {
      case hs: Span.HasStart => playTxn(hs.start)
      case _ =>
    }

    def guiInit(initPlaying: Boolean, initMillis: Long, hasMillis: Boolean, hasLoop: Boolean,
                hasShortcuts: Boolean): Unit = {
      val timeDisplay = TimeDisplay(timelineModel, hasMillis = hasMillis)

      val actions0 = Vector(
        GoToBegin   { rtz() },
        Rewind      { () },     // handled below
        Stop        { stop() },
        Play        { play() },
        FastForward { () }      // handled below
      )
      val actions1 = if (hasLoop) actions0 :+ Loop { toggleLoop() } else actions0
      transportStrip = GUITransport.makeButtonStrip(actions1)
      val initPressed = if (initPlaying) Play else Stop
      transportStrip.button(initPressed).foreach(_.selected = true)

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents += timeDisplay.component
        contents += HStrut(8)
        contents += transportStrip
      }

      if (hasShortcuts) {
        transportPane.addAction("play-stop", focus = FocusType.Window, action = new Action("play-stop") {
          accelerator = Some(KeyStrokes.plain + Key.Space)
          def apply(): Unit = playOrStop()
        })
        transportPane.addAction("rtz", focus = FocusType.Window, action = new Action("rtz") {
          accelerator = Some(KeyStrokes.plain + Key.Enter)
          enabled     = modOpt.isDefined
          def apply(): Unit =
            transportStrip.button(GoToBegin).foreach(_.doClick())
        })
      }

      playTimer = new javax.swing.Timer(47,
        Swing.ActionListener(modOpt.fold((_: ActionEvent) => ()) { mod => (e: ActionEvent) =>
          val elapsed = ((System.currentTimeMillis() - timerSys) * srm).toLong
          mod.position = timerFrame + elapsed
        })
      )

      cueTimer = new javax.swing.Timer(63, new ActionListener {
        def actionPerformed(e: ActionEvent): Unit = modOpt.foreach { mod =>
          val isPlaying = atomic { implicit tx => transport.isPlaying }
          if (!isPlaying) {
            mod.position = mod.position + (TimeRef.SampleRate * 0.25 * cueDirection).toLong
          }
        }
      })

      configureCueButton(Rewind     , -1)
      configureCueButton(FastForward, +1)

      component = transportPane
    }

    private def configureCueButton(act: GUITransport.Element, dir: Int): Unit =
      transportStrip.button(act).foreach { b =>
        val m = b.peer.getModel
        m.addChangeListener(new ChangeListener {
          var pressed = false

          def stateChanged(e: ChangeEvent): Unit = {
            val p = m.isPressed
            if (p != pressed) {
              pressed = p
              if (p) {
                //println( "-restart" )
                cueDirection = dir
                cueTimer.restart()
              } else {
                //println( "-stop" )
                cueTimer.stop()
              }
            }
          }
        })
      }
  }
}
