/*
 *  SoundProcesses.scala
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

package de.sciss.proc

import de.sciss.log.Logger
import de.sciss.lucre
import de.sciss.lucre.expr.LucreExpr
import de.sciss.lucre.synth.Executor
import de.sciss.lucre.{Cursor, Txn}
import de.sciss.synth.StandardUGens
import de.sciss.synth.proc.ProcUGens

import scala.concurrent.Future
import scala.concurrent.stm.{InTxn, Txn => STMTxn}

object SoundProcesses  {
  private[proc] def isPowerOfTwo(value: Int): Boolean = (value & (value - 1)) == 0

  private[proc] def validateCueBufferSize(value: Int): Unit =
    if (!isPowerOfTwo(value) || value < 8192 || value > 131072)
      throw new IllegalArgumentException(s"Must be a power of two and in (8192, 131072) : $value")

  /** Spawns a transactional function on the default `executionContext`. Throws
    * an exception if this method is called within a transaction.
    *
    * '''Warning:''' This returns a future, which '''must''' be observed to
    * handle errors that happen during the transaction. If the `Future` is ignored,
    * the safer way is to call `step` which invokes an error handler in that case.
    */
  def atomic[T <: Txn[T], A](fun: T => A)(implicit cursor: Cursor[T]): Future[A] =
    noTxnFuture("atomic") {
      cursor.step(fun)
    }

  // called from `step` if an error occurs, passing the context name and the error.
  var errorHandler: (String, Throwable) => Unit = {
    (ctx, t) =>
      Console.err.println(s"From '$ctx'")
      t.printStackTrace()
  }

  private def noTxnFuture[A](context: String)(body: => A): Future[A] = {
    val opt = STMTxn.findCurrent
    if (opt.isDefined) {
      implicit val tx: InTxn = opt.get
      val status = STMTxn.status
      log.warn(s"SoundProcesses.step. Existing transaction $opt - status is $status")
      if (!status.completed) {
        throw new IllegalStateException("Cannot nest transactions")
      }
    }
    Future {
      body
    } (Executor.executionContext)
  }

  /** Spawns a transactional function on the default `executionContext`. Throws
    * an exception if this method is called within a transaction.
    *
    * If an error occurs within the `fun`, `errorHandler` is invoked with the
    * `context` string argument and the error.
    */
  def step[T <: Txn[T]](context: String)(fun: T => Unit)(implicit cursor: Cursor[T]): Unit = {
    noTxnFuture(context) {
      try {
        cursor.step(fun)
      } catch {
        case t: Throwable =>
          errorHandler(context, t)
      }
    }
    ()
  }

  def stepTag[T <: Txn[T]](context: String)(fun: T => Unit)(implicit scheduler: Scheduler[T]): Unit = {
    noTxnFuture(context) {
      try {
        scheduler.stepTag(fun)
      } catch {
        case t: Throwable =>
          errorHandler(context, t)
      }
    }
    ()
  }

  final val log         : Logger = new Logger("proc")
  final val logAural    : Logger = new Logger("proc aural")
  final val logTransport: Logger = new Logger("proc transport")

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
    Tag           .init()
    Timeline      .init()
    Markdown      .init()
    EnvSegment    .init()

    lucre.expr.graph.AudioCue .init()
    lucre.expr.graph.Timeline .init()

    StandardUGens   .init()
    ProcUGens       .init()
    // ThirdPartyUGens .init()

    // initPlatform()
  }

  /** Registers all known types. */
  def init(): Unit = _init
}