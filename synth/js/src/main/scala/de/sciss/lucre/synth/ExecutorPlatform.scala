/*
 *  ExecutorPlatform.scala
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

package de.sciss.lucre.synth

import java.util.concurrent.TimeUnit

import de.sciss.lucre.synth.Executor.Cancelable

import scala.concurrent.ExecutionContext
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
    () => js.timers.clearTimeout(handle)
  }

  implicit def executionContext: ExecutionContext = ExecutionContext.global

  def isShutdown: Boolean = false // XXX TODO is this always true?
}
