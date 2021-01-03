/*
 *  Executor.scala
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

import scala.concurrent.{Future, Promise, TimeoutException}
import scala.util.Failure

object Executor extends ExecutorPlatform {
  trait Cancelable {
    def cancel(): Unit
  }

  /** If the future `f` completes within the given time, succeeds, otherwise
    * completes with a `scala.concurrent.TimeoutException`.
    */
  def timeOut[A](f: Future[A], time: Long, unit: TimeUnit): Future[A] = if (f.isCompleted) f else {
    val pr = Promise[A]()
    val timeOut = scheduleWithCancel(time, unit) {
      pr.failure(new TimeoutException)
    }
    f.onComplete {
      case tr @ Failure(_: TimeoutException) =>
        pr.complete(tr)
      case tr =>
        timeOut.cancel()
        pr.complete(tr)
    }
    pr.future
  }
}
