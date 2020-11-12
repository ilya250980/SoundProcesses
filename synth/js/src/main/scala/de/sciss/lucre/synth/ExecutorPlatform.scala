package de.sciss.lucre.synth

import java.util.concurrent.TimeUnit

import de.sciss.lucre.synth.Executor.Cancelable

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.Duration
import scala.scalajs.js

trait ExecutorPlatform {
  def defer(body: => Unit): Unit =
    executionContext.execute(() => body)
//    js.timers.setTimeout(0.0)(body)

  def schedule(time: Long, unit: TimeUnit)(body: => Unit): Unit = {
    js.timers.setTimeout(Duration(time, unit))(body)
    ()
  }

  def scheduleWithCancel(time: Long, unit: TimeUnit)(body: => Unit): Cancelable = {
    val handle = js.timers.setTimeout(Duration(time, unit))(body)
    new Cancelable {
      def cancel(): Unit = js.timers.clearTimeout(handle)
    }
  }

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  def isShutdown: Boolean = false // XXX TODO is this always true?
}
