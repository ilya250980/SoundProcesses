/*
 *  SoundProcesses.scala
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

package de.sciss.synth.proc

import de.sciss.lucre
import de.sciss.lucre.expr.LucreExpr
import de.sciss.lucre.synth.Executor
import de.sciss.lucre.{Cursor, Txn}

import scala.concurrent.Future
import scala.concurrent.stm.{Txn => STMTxn}

object SoundProcesses extends SoundProcessesPlatform {
//  /** Returns the size of the thread pool used in `atomic`,
//    * where `None` indicates that a single-threaded context is used
//    * (default).
//    */
//  def poolSize: Option[Int] = NodeImpl.poolSize
//
//  /** Sets the size of the thread pool used in `atomic`.
//    * Note that this is only effective until the moment that
//    * pool has been used for the first time (e.g. by invocation
//    * of `atomic` or on a node's `onEnd`. Therefore this method
//    * should only be used during application startup.
//    *
//    * A `Some` value specifies the number of concurrent threads,
//    * a `None` value is equivalent to a single-threaded context.
//    */
//  def poolSize_=(value: Option[Int]): Unit  = NodeImpl.poolSize = value

  private[proc] def isPowerOfTwo(value: Int): Boolean = (value & (value - 1)) == 0

  private[proc] def validateCueBufferSize(value: Int): Unit =
    if (!isPowerOfTwo(value) || value < 8192 || value > 131072)
      throw new IllegalArgumentException(s"Must be a power of two and in (8192, 131072) : $value")

//  /** Same as `lucre.synth.impl.NodeImpl.pool`. */
//  def scheduledExecutorService: ScheduledExecutorService = NodeImpl.pool

//  /** Default execution-context used for scheduling and spawning functions.
//    * It uses the `scheduledExecutorService`.
//    */
//  lazy implicit val executionContext: ExecutionContext =
//    ExecutionContext.fromExecutorService(scheduledExecutorService)

  /** Spawns a transactional function on the default `executionContext`. Throws
    * an exception if this method is called within a transaction.
    *
    * '''Warning:''' This returns a future, which '''must''' be observed to
    * handle errors that happen during the transaction. If the `Future` is ignored,
    * the safer way is to call `step` which invokes an error handler in that case.
    */
  def atomic[T <: Txn[T], A](fun: T => A)(implicit cursor: Cursor[T]): Future[A] = {
    if (STMTxn.findCurrent.isDefined) throw new IllegalStateException("Cannot nest transactions")
    Future {
      cursor.step(fun)
    } (Executor.context)
  }

  // called from `step` if an error occurs, passing the context name and the error.
  var errorHandler: (String, Throwable) => Unit = {
    (ctx, t) =>
      Console.err.println(s"From '$ctx'")
      t.printStackTrace()
  }

  /** Spawns a transactional function on the default `executionContext`. Throws
    * an exception if this method is called within a transaction.
    *
    * If an error occurs within the `fun`, `errorHandler` is invoked with the
    * `context` string argument and the error.
    */
  def step[T <: Txn[T]](context: String)(fun: T => Unit)(implicit cursor: Cursor[T]): Unit = {
    if (STMTxn.findCurrent.isDefined) throw new IllegalStateException("Cannot nest transactions")
    Future {
      try {
        cursor.step(fun)
      } catch {
        case t: Throwable =>
          errorHandler(context, t)
      }
    } (Executor.context)
    ()
  }

  private[this] lazy val _init: Unit = {
    LucreExpr     .init()
    Code          .init()
    Color         .init()
    Control       .init()
    Action        .init()
    AudioCue      .init()
    Cursors       .init()
    CurveObj      .init()
    FadeSpec      .init()
    Grapheme      .init()
    Proc          .init()
    SynthGraphObj .init()
    Timeline      .init()
    Markdown      .init()
    EnvSegment    .init()

    lucre.expr.graph.AudioCue .init()
    lucre.expr.graph.Timeline .init()

    initPlatform()
  }

  /** Registers all known types. */
  def init(): Unit = _init
}