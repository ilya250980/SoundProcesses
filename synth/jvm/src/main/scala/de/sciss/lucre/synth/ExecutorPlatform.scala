package de.sciss.lucre.synth

import java.util.concurrent.{Executors, ScheduledExecutorService, TimeUnit}

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

  def defer(body: => Unit): Unit =
    pool.submit((() => body): Runnable)

  def schedule(time: Long, unit: TimeUnit)(body: => Unit): Unit =
    pool.schedule((() => body): Runnable, time, unit)

  /** Default execution-context used for scheduling and spawning functions.
    * It uses the `scheduledExecutorService`.
    */
  implicit def context: ExecutionContext = _context

  private lazy val _context: ExecutionContext =
    ExecutionContext.fromExecutorService(pool)

  private def shutdownScheduler(): Unit = {
    Log.log("Shutting down scheduler thread pool")
    pool.shutdown()
  }
}
