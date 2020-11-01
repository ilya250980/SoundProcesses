/*
 *  TransportViewImpl.scala
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

package de.sciss.synth.proc.gui.impl

import java.awt.event.{ActionEvent, ActionListener, InputEvent}

import de.sciss.audiowidgets.{TimelineModel, Transport => GUITransport}
import de.sciss.desktop.Implicits._
import de.sciss.desktop.{FocusType, KeyStrokes}
import de.sciss.lucre.swing.LucreSwing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.{Cursor, Disposable, synth}
import de.sciss.span.Span
import de.sciss.synth.proc.gui.{TimeDisplay, TransportView}
import de.sciss.synth.proc.{TimeRef, Transport}
import javax.swing.event.{ChangeEvent, ChangeListener}
import javax.swing.{AbstractAction, ButtonModel, JComponent, KeyStroke, Timer}

import scala.concurrent.stm.Ref
import scala.swing.Swing.HStrut
import scala.swing.event.Key
import scala.swing.{Action, BoxPanel, Component, Orientation, Swing}

object TransportViewImpl {
  def apply[T <: synth.Txn[T]](transport: Transport[T] /* .Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */ ,
                         model: TimelineModel, hasMillis: Boolean, hasLoop: Boolean, hasShortcuts: Boolean)
                        (implicit tx: T, cursor: Cursor[T]): TransportView[T] = {
    val view    = new Impl(transport, model)
    // val srk     = 1000 / TimeRef.SampleRate // transport.sampleRate

    view.observer = transport.react { implicit tx => {
      case Transport.Play(_, time) => view.startedPlaying(time)
      case Transport.Stop(_, time) => view.stoppedPlaying(time)
      case Transport.Seek(_, time, p) =>
        if (p) view.startedPlaying(time) else view.stoppedPlaying(time)
      case _ =>
    }}

    val initPlaying = transport.isPlaying // .playing.value
    // val initMillis = (transport.position * srk).toLong
    deferTx {
      view.guiInit(initPlaying, /*initMillis,*/ hasMillis = hasMillis, hasLoop = hasLoop, hasShortcuts = hasShortcuts)
    }
    view
  }

//  private final class Foo(m: ButtonModel, dir: Int) extends ChangeListener {
//    var pressed = false
//
//    def stateChanged(e: ChangeEvent): Unit = {
//      val p = m.isPressed
//      if (p != pressed) {
//        pressed = p
//        if (p) {
//          //println( "-restart" )
//          cueDirection = dir
//          cueTimer.restart()
//        } else {
//          //println( "-stop" )
//          cueTimer.stop()
//        }
//      }
//    }
//  }

  // (0 until 20).map(i => (math.sin(i.linlin(0, 20, 0, math.Pi)) * 7.20).round).scanLeft(10.0) { case (sum, i) => sum + i }
  private final val cueSteps = Array[Float](
      0.010f, 0.010f, 0.011f, 0.013f, 0.016f, 0.020f, 0.025f, 0.031f, 0.037f, 0.044f,
      0.051f, 0.058f, 0.065f, 0.072f, 0.078f, 0.084f, 0.089f, 0.093f, 0.096f, 0.098f, 0.099f)

  private final class Impl[T <: synth.Txn[T]](val transport: Transport[T] /* .Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]] */ ,
                                        val timelineModel: TimelineModel)
                                       (implicit protected val cursor: Cursor[T])
    extends TransportView[T] with ComponentHolder[Component] with CursorHolder[T] {

    type C = Component

    var observer: Disposable[T] = _

    private[this] val modOpt  = timelineModel.modifiableOption

    private[this] var playTimer   : Timer = _
    private[this] var cueTimer    : Timer = _
    private[this] var cueDirection  = 1
    private[this] var cueCount      = 0
    private[this] var cueSelect     = false

    private[this] var timerFrame  = 0L
    private[this] var timerSys    = 0L
    private[this] val srm         = 0.001 * TimeRef.SampleRate // transport.sampleRate

    private[this] var transportStrip: Component with GUITransport.ButtonStrip = _

    private[this] val loopSpan  = Ref[Span.SpanOrVoid](Span.Void)
    private[this] val loopToken = Ref(-1)

    import GUITransport.{FastForward, GoToBegin, Loop, Play, Rewind, Stop}
    import transport.universe.scheduler.stepTag

    private final class ActionCue(elem: GUITransport.Element, key: Key.Value, onOff: Boolean, select: Boolean)
      extends AbstractAction(elem.toString) { action =>

//      private[this] var lastWhen: Long = 0L

      private[this] val b = transportStrip.button(elem).get

      b.peer.registerKeyboardAction(this, s"${if (onOff) "start" else "stop"}-${elem.toString}",
        KeyStroke.getKeyStroke(key.id, if (select) InputEvent.SHIFT_MASK else 0, !onOff),
        JComponent.WHEN_IN_FOCUSED_WINDOW)

      private def perform(): Unit = {
        val bm = b.peer.getModel
        cueSelect = select
        if (bm.isPressed != onOff) bm.setPressed(onOff)
        if (bm.isArmed   != onOff) bm.setArmed  (onOff)
      }

      def actionPerformed(e: ActionEvent): Unit = {
        // println(s"actionPerformed ${elem.toString}; onOff = $onOff")
//        lastWhen = e.getWhen
        perform()
      }
    }

    private final class CueListener(elem: GUITransport.Element, dir: Int)
      extends ChangeListener {

      private[this] val m: ButtonModel  = transportStrip.button(elem).get.peer.getModel
      private[this] var pressed         = false
      private[this] var wasPlaying      = false

      m.addChangeListener(this)

      def stateChanged(e: ChangeEvent): Unit = {
        val p = m.isPressed
        if (p != pressed) {
          pressed = p
          if (p) {
            cueDirection = dir
            wasPlaying   = transportStrip.button(Play).exists(_.selected)
            // start at higher cue speed if direction is
            // forward and transport was playing,
            // because otherwise it appears sluggish
            cueCount     = if (wasPlaying /* && dir > 0 */) 10 else 0
            if (wasPlaying) stop()
            cueTimer.restart()
          } else {
            cueTimer.stop()
            if (wasPlaying) play()
          }
        }
      }
    }

    def dispose()(implicit tx: T): Unit = {
      observer.dispose()
      cancelLoop()
      deferTx {
        playTimer.stop()
        cueTimer .stop()
      }
    }

    private def cancelLoop()(implicit tx: T): Unit =
      transport.universe.scheduler.cancel(loopToken.swap(-1)(tx.peer))

    def startedPlaying(time: Long)(implicit tx: T): Unit = {
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

    def stoppedPlaying(time: Long)(implicit tx: T): Unit = {
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
      for {
        mod   <- modOpt
        start <- mod.bounds.startOption
      } {
        mod.position  = start
        mod.visible   = Span(start, start + mod.visible.length)
      }
    }

    private def playOrStop(): Unit = {
      // we have both visual feedback
      // and transactional control here,
      // because we want to see the buttons
      // depressed, but we also want zero
      // latency in taking action.
      val isPlaying =
        (for {
          ggStop <- transportStrip.button(Stop)
          ggPlay <- transportStrip.button(Play)
        } yield {
          val _isPlaying = ggPlay.selected
          val but = if (_isPlaying) ggStop else ggPlay
          but.doClick()
          _isPlaying
        }).getOrElse(false)

      stepTag { implicit tx =>
        if (isPlaying)
          transport.stop()
        else
          playTxn()
      }
    }

    private def stop(): Unit = atomic { implicit tx =>
      transport.stop()
    }

    private def play(): Unit = stepTag { implicit tx =>
      playTxn()
    }

    private def playTxn(pos: Long = timelineModel.position)(implicit tx: T): Unit =
      if (!transport.isPlaying) {
//        transport.stop()
        transport.seek(pos  )
        transport.play()
      }

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

    private def checkLoop()(implicit tx: T): Unit = {
      val pos       = transport.position
      val loopStop  = loopSpan.get(tx.peer) match { case hs: Span.HasStop => hs.stop; case _ => Long.MinValue }
      if (loopStop > pos) {
        val sch     = transport.universe.scheduler
        val time    = sch.time + (loopStop - pos)
        val token   = sch.schedule(time) { implicit tx => loopEndReached() }
        val old     = loopToken.swap(token)(tx.peer)
        sch.cancel(old)
      }
    }

    private def loopEndReached()(implicit tx: T): Unit = loopSpan.get(tx.peer) match {
      case hs: Span.HasStart => playTxn(hs.start)
      case _ =>
    }

    def guiInit(initPlaying: Boolean, /*initMillis: Long,*/ hasMillis: Boolean, hasLoop: Boolean,
                hasShortcuts: Boolean): Unit = {
      val timeDisplay = TimeDisplay(timelineModel, hasMillis = hasMillis)

      val actions0 = Vector(
        Rewind      { () },     // handled below
        Stop        { stop() },
        Play        { play() },
        FastForward { () }      // handled below
      )
      val actions1 = if (timelineModel.bounds.startOption.isEmpty) actions0 else GoToBegin(rtz()) +: actions0
      val actions2 = if (!hasLoop) actions1 else actions1 :+ Loop(toggleLoop())
      transportStrip = GUITransport.makeButtonStrip(actions2)
      val initPressed = if (initPlaying) Play else Stop
      transportStrip.button(initPressed).foreach(_.selected = true)

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents += timeDisplay.component
        contents += HStrut(8)
        contents += transportStrip
      }

      def addClickAction(name: String, key: Key.Value, elem: GUITransport.Element): Unit =
        transportPane.addAction(name, focus = FocusType.Window, action = new Action(name) {
          accelerator = Some(KeyStrokes.plain + key)
          enabled     = modOpt.isDefined
          def apply(): Unit =
            transportStrip.button(elem).foreach(_.doClick())
        })

      if (hasShortcuts) {
        transportPane.addAction("play-stop", focus = FocusType.Window, action = new Action("play-stop") {
          accelerator = Some(KeyStrokes.plain + Key.Space)
          def apply(): Unit = playOrStop()
        })
        addClickAction("rtz" , Key.Enter, GoToBegin)
        addClickAction("loop", Key.Slash, Loop     )

        mkCue(Rewind      , Key.OpenBracket )
        mkCue(FastForward , Key.CloseBracket)
      }

      playTimer = new javax.swing.Timer(27 /* 47 */,
        Swing.ActionListener(modOpt.fold((_: ActionEvent) => ()) { mod => (_: ActionEvent) =>
          val elapsed = ((System.currentTimeMillis() - timerSys) * srm).toLong
          mod.position = timerFrame + elapsed
        })
      )

      cueTimer = new javax.swing.Timer(25 /* 63 */, new ActionListener {
        def actionPerformed(e: ActionEvent): Unit = modOpt.foreach { mod =>
          val isPlaying = atomic { implicit tx => transport.isPlaying }
          if (!isPlaying) {
            val cc = cueCount
            cueCount = cc + 1
            val inc =
//              if (cc < 14) {
//                math.max(1, cc - 4) * 10
              if (cc < 21) {
                cueSteps(cc)
              } else if (cc < 210 /* 209 */) {
                0.1f
              } else {
                0.5f
              }
            val d         = (TimeRef.SampleRate * inc * cueDirection).toLong
            val posOld    = mod.position
            val posNew    = posOld + d
            mod.position  = posNew
            if (cueSelect) {
              def mkSpan(a: Long, b: Long) = Span(math.min(a, b), math.max(a, b))

              val selNew = mod.selection match {
                case Span.Void => mkSpan(posOld, posNew)
                case Span(start, stop) =>
                  if (math.abs(posNew - start) < math.abs(posNew - stop))
                    mkSpan(stop, posNew)
                  else
                    mkSpan(start, posNew)
              }
              mod.selection = selNew
            }
          }
        }
      })

      new CueListener(Rewind     , -1)
      new CueListener(FastForward, +1)

      component = transportPane
    }

    private def mkCue(elem: GUITransport.Element, key: Key.Value): Unit = {
      new ActionCue(elem, key, onOff = false, select = false)
      new ActionCue(elem, key, onOff = true , select = false)
      new ActionCue(elem, key, onOff = false, select = true )
      new ActionCue(elem, key, onOff = true , select = true )
      ()
    }
  }
}