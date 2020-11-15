/*
 *  Scheduler.scala
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

package de.sciss.proc

import de.sciss.lucre.{Cursor, Txn}
import de.sciss.proc.impl.{SchedulerImpl => Impl}

object Scheduler {
  /** Creates a real-time scheduler. */
  def apply[T <: Txn[T]]()(implicit tx: T, cursor: Cursor[T]): Scheduler[T] = Impl[T]()

  /** Creates a non-real-time scheduler. */
  def offline[T <: Txn[T]](implicit tx: T, cursor: Cursor[T]): Offline[T] = Impl.offline[T]

  trait Offline[T <: Txn[T]] extends Scheduler[T] {
    def step()    (implicit tx: T): Unit
    def stepTarget(implicit tx: T): Option[Long]
  }

  final case class Entry[T <: Txn[T]](time: Long, fun: T => Unit)
}

/** A `Scheduler` uses a logical frame clock to execute functions transactionally
  * at specific times. It is parametrized in `S` in order to perform transactions,
  * but it does not store any state that would need the scheduler to be handled
  * with `stm.Source`. It can be safely stored in a regular value.
  */
trait Scheduler[T <: Txn[T]] {
  /** Logical time frame based on `TimeRef.SampleRate` and with zero
    * corresponding to creation time. Frames elapsed with wall-clock
    * but are stable within a transaction.
    */
  def time(implicit tx: T): Long

  /** Performs a tagged transaction step.
    *
    * @see [[de.sciss.lucre.Cursor.stepTag]]
    */
  def stepTag[A](fun: T => A): A

  /** Schedules the execution of a function at a given time. Time is given
    * as an "absolute" frame in the sense of `AuralContext.time`.
    * Returns a token that can be used to cancel the action.
    * The token is `>= 0`.
    */
  def schedule(time: Long)(fun: T => Unit)(implicit tx: T): Int /* Token */

  /** Cancels a scheduled action.
    * It is ok to use an old token that was already completed or cancelled.
    */
  def cancel(token: Int /* Token */)(implicit tx: T): Unit

  implicit def cursor: Cursor[T]
}