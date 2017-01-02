/*
 *  Scheduler.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import impl.{SchedulerImpl => Impl}

object Scheduler {
  /** Creates a real-time scheduler. */
  def apply  [S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Scheduler[S] = Impl[S]

  /** Creates a non-real-time scheduler. */
  def offline[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Offline[S] = Impl.offline[S]

  trait Offline[S <: Sys[S]] extends Scheduler[S] {
    def step()    (implicit tx: S#Tx): Unit
    def stepTarget(implicit tx: S#Tx): Option[Long]
  }

  final case class Entry[S <: Sys[S]](time: Long, fun: S#Tx => Unit)
}

/** A `Scheduler` uses a logical frame clock to execute functions transactionally
  * at specific times. It is parametrized in `S` in order to perform transactions,
  * but it does not store any state that would need the scheduler to be handled
  * with `stm.Source`. It can be safely stored in a regular value.
  */
trait Scheduler[S <: Sys[S]] {
  /** Logical time frame based on `Timeline.SampleRate` and with zero
    * corresponding to creation time. Frames elapsed with wall-clock
    * but are stable within a transaction.
    */
  def time(implicit tx: S#Tx): Long

  /** Schedules the execution of a function at a given time. Time is given
    * as an "absolute" frame in the sense of `AuralContext.time`.
    * Returns a token that can be used to cancel the action.
    * The token is `>= 0`.
    */
  def schedule(time: Long)(fun: S#Tx => Unit)(implicit tx: S#Tx): Int /* Token */

  /** Cancels a scheduled action.
    * It is ok to use an old token that was already completed or cancelled.
    */
  def cancel(token: Int /* Token */)(implicit tx: S#Tx): Unit

  implicit def cursor: stm.Cursor[S]
}