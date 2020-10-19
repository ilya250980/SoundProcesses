package de.sciss.lucre.synth

import java.util.concurrent.TimeUnit

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.scalajs.js

trait ExecutorPlatform {
  def defer(body: => Unit): Unit =
    context.execute(() => body)
//    js.timers.setTimeout(0.0)(body)

  def schedule(time: Long, unit: TimeUnit)(body: => Unit): Unit =
    js.timers.setTimeout(Duration(time, unit))(body)

  implicit def context: ExecutionContext = ExecutionContext.global

  def isShutdown: Boolean = false // XXX TODO is this always true?
}
