/*
 *  SoundProcesses.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.util.concurrent.ScheduledExecutorService

import de.sciss.lucre
import de.sciss.lucre.expr.LucreExpr
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.impl.NodeImpl

import scala.concurrent.stm.Txn
import scala.concurrent.{ExecutionContext, Future}

object SoundProcesses {
  /** Returns the size of the thread pool used in `atomic`,
    * where `None` indicates that a single-threaded context is used
    * (default).
    */
  def poolSize: Option[Int] = NodeImpl.poolSize

  /** Sets the size of the thread pool used in `atomic`.
    * Note that this is only effective until the moment that
    * pool has been used for the first time (e.g. by invocation
    * of `atomic` or on a node's `onEnd`. Therefore this method
    * should only be used during application startup.
    *
    * A `Some` value specifies the number of concurrent threads,
    * a `None` value is equivalent to a single-threaded context.
    */
  def poolSize_=(value: Option[Int]): Unit  = NodeImpl.poolSize = value

  private[proc] def isPowerOfTwo(value: Int): Boolean = (value & (value - 1)) == 0

  private[proc] def validateCueBufferSize(value: Int): Unit =
    if (!isPowerOfTwo(value) || value < 8192 || value > 131072)
      throw new IllegalArgumentException(s"Must be a power of two and in (8192, 131072) : $value")

  /** Same as `lucre.synth.impl.NodeImpl.pool`. */
  def scheduledExecutorService: ScheduledExecutorService = NodeImpl.pool

  /** Default execution-context used for scheduling and spawning functions.
    * It uses the `scheduledExecutorService`.
    */
  lazy implicit val executionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(scheduledExecutorService)

  /** Spawns a transactional function on the default `executionContext`. Throws
    * an exception if this method is called within a transaction.
    */
  def atomic[S <: Sys[S], A](fun: S#Tx => A)(implicit cursor: stm.Cursor[S]): Future[A] = {
    if (Txn.findCurrent.isDefined) throw new IllegalStateException("Cannot nest transactions")
    Future {
      cursor.step(fun)
    } (executionContext)
  }

  private[this] lazy val _init: Unit = {
    LucreExpr     .init()
    ActionRaw        .init()
    AudioCue      .init()
    Code          .init()
    Color         .init()
    Control       .init()
    Cursors       .init()
    CurveObj      .init()
    Ensemble      .init()
    FadeSpec      .init()
//    Folder        .init()
    Grapheme      .init()
    Output        .init()
    Proc          .init()
    SynthGraphObj .init()
    Timeline      .init()
    Markdown      .init()
    EnvSegment    .init()

    lucre.expr.graph.Artifact .init()
    lucre.expr.graph.Folder   .init()
    lucre.expr.graph.Timeline .init()
  }

  /** Registers all known types. */
  def init(): Unit = _init
}