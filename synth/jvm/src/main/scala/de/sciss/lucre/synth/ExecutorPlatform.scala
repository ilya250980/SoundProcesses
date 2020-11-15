/*
 *  ExecutorPlatform.scala
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

package de.sciss.lucre.synth

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

import de.sciss.lucre.synth.Executor.Cancelable
import de.sciss.lucre.Log.{synth => log}

import scala.concurrent.ExecutionContext

trait ExecutorPlatform {
  private lazy val pool: ScheduledExecutorService = {
    // system wide scheduler
    val res =
    //      poolSize match {
    //        case Some(sz) => Executors.newScheduledThreadPool(sz)
    //        case _        =>
      Executors.newSingleThreadScheduledExecutor()
    //      }

    sys.addShutdownHook(shutdownScheduler())
    res
  }

  def isShutdown: Boolean = pool.isShutdown

  def defer(body: => Unit): Unit = {
    pool.submit((() => body): Runnable)
    ()
  }

  def schedule(time: Long, unit: TimeUnit)(body: => Unit): Unit = {
    pool.schedule((() => body): Runnable, time, unit)
    ()
  }

  def scheduleWithCancel(time: Long, unit: TimeUnit)(body: => Unit): Cancelable = {
    val fut = pool.schedule((() => body): Runnable, time, unit)
    new Cancelable {
      def cancel(): Unit = {
        fut.cancel(false)
        ()
      }
    }
  }

  /** Default execution-context used for scheduling and spawning functions.
    * It uses the `scheduledExecutorService`.
    */
  implicit def executionContext: ExecutionContext = _context

  private lazy val _context: ExecutionContext =
    ExecutionContext.fromExecutorService(pool)

  private def shutdownScheduler(): Unit = {
    log.info("Shutting down scheduler thread pool")
    pool.shutdown()
  }
}
