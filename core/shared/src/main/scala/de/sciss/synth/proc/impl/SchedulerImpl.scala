/*
 *  SchedulerImpl.scala
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

package de.sciss.synth.proc.impl

import java.util.concurrent.TimeUnit

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.synth.Executor
import de.sciss.lucre.{Cursor, Txn}
import de.sciss.serial.ConstFormat
import de.sciss.synth.proc.Scheduler.Entry
import de.sciss.synth.proc.{Scheduler, TimeRef}
import de.sciss.synth.proc.SoundProcesses.{logTransport => logT}

import scala.concurrent.stm.{InTxn, Ref, TMap, TxnLocal}
import scala.util.control.NonFatal

object SchedulerImpl {
  def apply[T <: Txn[T]]()(implicit tx: T, cursor: Cursor[T]): Scheduler[T] = {
    val pq = mkPriorityQueue[T, tx.I](tx, tx.inMemoryBridge)
    new RealtimeImpl[T, tx.I](pq)(cursor, tx.inMemoryBridge)
  }

  def offline[T <: Txn[T]](implicit tx: T, cursor: Cursor[T]): Scheduler.Offline[T] = {
    val pq = mkPriorityQueue[T, tx.I](tx, tx.inMemoryBridge)
    new OfflineImpl[T, tx.I](pq)(cursor, tx.inMemoryBridge)
  }

  private def mkPriorityQueue[T <: Txn[T], I1 <: Txn[I1]](implicit tx: T, iSys: T => I1): SkipList.Map[I1, Long, Set[Int]] = {
    implicit val itx: I1 = iSys(tx)
    implicit val setFmt: ConstFormat[Set[Int]] = ConstFormat.set
    SkipList.Map.empty[I1, Long, Set[Int]]()
  }

  /* Information about the current situation of the scheduler.
   *
   * @param issueTime          the CPU time in sample frames at which the info was last updated
   * @param targetTime         the next frame at which a significant event happens in terms
   */
  private final class Info(val issueTime: Long, val targetTime: Long) {
    def delay: Long = targetTime - issueTime

    def isInf: Boolean = targetTime == Long.MaxValue

    import de.sciss.synth.proc.TimeRef.{framesAndSecs => fas}
    override def toString = s"[issueTime = ${fas(issueTime)}, targetTime = ${fas(targetTime)}]"
  }

  private val infInfo = new Info(issueTime = 0L, targetTime = Long.MaxValue)

  private final class OfflineImpl[T <: Txn[T], I <: Txn[I]](protected val pq: SkipList.Map [I , Long, Set[Int]])
                                                           (implicit val cursor: Cursor[T],
                                                            protected val iSys: T => I)
    extends Impl[T, I] with Scheduler.Offline[T] {

    private val timeRef = Ref(0L)

    def           time               (implicit tx: T): Long = timeRef.get(tx.peer)
    protected def time_=(value: Long)(implicit tx: T): Unit = timeRef.set(value)(tx.peer)

//    def systemTimeNanos(implicit tx: T): Long = calcTimeNanos(time)

//    private def calcTimeNanos(frames: Long): Long = {
//      val framesNanos = (frames / sampleRateN).toLong
//      framesNanos
//    }

    def stepTag[A](fun: T => A): A = cursor.step(fun)

    protected def submit(info: Info)(implicit tx: T): Unit =
      infoVar.set(info)(tx.peer)

    def step()(implicit tx: T): Unit = {
      val info = infoVar.get(tx.peer)
      if (!info.isInf) eventReached(info)
    }

    def stepTarget(implicit tx: T): Option[Long] = {
      implicit val ptx: InTxn = tx.peer
      val info = infoVar()
      if (info.isInf) None else Some(info.targetTime)
    }
  }

  private final class RealtimeImpl[T <: Txn[T], I <: Txn[I]](protected val pq: SkipList.Map [I , Long, Set[Int]])
                                                            (implicit val cursor: Cursor[T],
                                                             protected val iSys: T => I)
    extends Impl[T, I] {

    /*
      Timing information:

      - we can't get nanoseconds and milliseconds atomically at the same time (can we?), therefore
        set nanoseconds to zero for the absolute system time reference `timeAbsNanoSec`
      - the relative nanoseconds are used to calculate frames in `calcFrame`
      - the `timeRef` freezes the advanced frames since creation time in sample frames
      - to convert this to system time, divide `timeRef` by the sample rate, multiply by 1.0e9,
        and add the absolute time reference.

     */

    private[this] val timeAbsNanoSec  = System.currentTimeMillis() * 1000000L
    private[this] val timeRelNanoSec  = System.nanoTime()
    private[this] val timeRef         = TxnLocal(calcFrame())

    def           time               (implicit tx: T): Long = timeRef.get(tx.peer)
    protected def time_=(value: Long)(implicit tx: T): Unit = timeRef.set(value)(tx.peer)

    private def calcTimeNanoSec(frames: Long): Long = {
      val framesNanoSec = (frames / sampleRateN).toLong
      timeAbsNanoSec + framesNanoSec
    }

    private def calcFrame(): Long = {
      // 1 ns = 10^-9 s
      val delta = System.nanoTime() - timeRelNanoSec
      (delta * sampleRateN).toLong
    }

    def stepTag[A](fun: T => A): A = {
      val nowFrames = calcFrame()
      val nowNanos  = calcTimeNanoSec(nowFrames)
      cursor.stepTag(nowNanos) { implicit tx =>
        time = nowFrames
        fun(tx)
      }
    }

    protected def submit(info: Info)(implicit tx: T): Unit = {
      implicit val ptx: InTxn = tx.peer
      infoVar()         = info
      val jitter        = calcFrame() - info.issueTime
      val actualDelayN  = math.max(0L, ((info.delay - jitter) / sampleRateN).toLong)
      logT.debug(f"scheduled:     $info; log dly = ${TimeRef.framesAndSecs(info.delay)}, act dly = ${actualDelayN * 1.0e-9}%1.3fs")
      tx.afterCommit {
        Executor.schedule(actualDelayN, TimeUnit.NANOSECONDS) {
          logT.debug(s"scheduled: exe $info")
          val nowNanos = calcTimeNanoSec(info.targetTime)
          cursor.stepTag(nowNanos) { implicit tx =>
            eventReached(info) // this calls `time_=(info.targetTime)`
          }
        }
      }
    }
  }

  // one can argue whether the values should be ordered, e.g. Seq[Int] instead of Set[Int],
  // such that if two functions A and B are submitted after another for the same target time,
  // then A would be executed before B. But currently we don't think this is an important aspect.
  private abstract class Impl[T <: Txn[T], I <: Txn[I]]
    extends Scheduler[T] {

    // ---- abstract ----

    protected def pq: SkipList.Map[I , Long, Set[Int]]
    protected def iSys: T => I

    /** Invoked to submit a schedule step either to a realtime scheduler or other mechanism.
      * When the step is performed, execution should be handed over to `eventReached`, passing
      * over the same three arguments.
      */
    protected def submit(info: Info)(implicit tx: T): Unit

    protected def time_=(value: Long)(implicit tx: T): Unit

    // ---- implemented ----

    private type Token = Int

    private val tokenRef    = Ref(0)
    private val tokenMap    = TMap.empty[Int, Entry[T]]

    final protected val sampleRateN = 0.014112 // = Timeline.SampleRate * 1.0e-9
    protected final val infoVar     = Ref(infInfo)

    // ---- scheduling ----

    final def schedule(targetTime: Long)(fun: T => Unit)(implicit tx: T): Token = {
      implicit val ptx: InTxn = tx.peer
      implicit val itx: I = iSys(tx)
      val t             = time
      if (targetTime < t) throw new IllegalArgumentException(s"Cannot schedule in the past ($targetTime < $time)")
      val token         = tokenRef.getAndTransform(_ + 1)
      tokenMap.put(token, new Entry[T](targetTime, fun))
      val oldInfo       = infoVar()
      val reschedule    = targetTime < oldInfo.targetTime

      if (reschedule) {   // implies that prio does not have an entry at `timeClip` yet
        // println(s"............... SCHEDULE t = $targetTime; token = $token - REPLACE")
        assert(!pq.contains(targetTime))
        pq.put(targetTime, Set(token))
      } else {
        val newSet = pq.get(targetTime).fold(Set(token))(_ + token)
        // println(s"............... SCHEDULE t = $targetTime; token = $token - ADD > $newSet")
        pq.put(targetTime, newSet)
      }

      import TimeRef.{framesAndSecs => fas}
      logT.debug(s"schedule: token = $token, time = ${fas(t)}, old tgt ${fas(oldInfo.targetTime)}, new tgt = ${fas(targetTime)}, submit? $reschedule")

      if (reschedule) {
        val newInfo = new Info(issueTime = t, targetTime = targetTime)
        submit(newInfo)
      }

      token
    }

    final def cancel(token: Token)(implicit tx: T): Unit = {
      implicit val ptx: InTxn = tx.peer
      implicit val itx: I = iSys(tx)
      //      tokenMap.remove(token).fold {
      //        Console.err.println(s"Trying to cancel an unregistered token $token")
      //      } { sch =>
      tokenMap.remove(token).foreach { sch =>
        val t     = sch.time
        // println(s"............... REMOVED TOKEN $token; t = $t")

        // NOTE: the following assertion is wrong.
        // because in `eventReached`, we remove from prio
        // first before iterating over the tokens. If
        // there are two tokens A, B and the function for A
        // cancels B (invoking `cancel`), then obviously
        // the `prio.get(t)` will return `None`.

//        val set0  = prio.get(t).getOrElse(
//          throw new AssertionError(s"Token $token found but no entry at $t in priority queue")
//        )
        pq.get(t).foreach { set0 =>
          val set1  = set0 - token
          if (set1.isEmpty) {
            // println(s"............... .... > EMPTY")
            pq.remove(t)
            // if entry became empty, see if it was
            // scheduled; if so, re-submit
            val info = infoVar()
            if (info.targetTime == t) scheduleNext()

          } else {
            // println(s"............... .... > $set1")
            pq.put(t, set1)
          }
        }
      }
    }

    /** Invoked from the `submit` body after the scheduled event is reached. */
    final protected def eventReached(info: Info)(implicit tx: T): Unit = {
      implicit val itx: I  = iSys(tx)
      implicit val ptx: InTxn = tx.peer
      if (info != infoVar()) return // the scheduled task was invalidated by an intermediate stop or seek command

      // this is crucial to eliminate drift: since we reached the scheduled event, do not
      // let the timeRef txn-local determine a new free wheeling time, but set it to the
      // time we targeted at; then in the next scheduleNext call, the jitter is properly
      // calculated.
      val t = info.targetTime
      time = t

      pq.remove(t).foreach { tokens =>
        // println(s"............... REMOVED PRIO t = $t; tokens = $tokens")
        tokens.foreach { token =>
          // println(s"............... .... >>>> TOKEN $token")
          tokenMap.remove(token).foreach { sched =>
            // println(s"............... .... TOKEN $token")
            try {
              sched.fun(tx)
            } catch {
              case NonFatal(e) =>
                Console.err.println(s"While executing scheduled function $token:")
                e.printStackTrace()
            }
          }
          // println(s"............... .... <<<< TOKEN $token")
        }
      }

      scheduleNext()
    }

    // looks at the smallest time on the queue. if it exists, submits to peer scheduler
    private def scheduleNext()(implicit tx: T): Unit = {
      implicit val itx: I = iSys(tx)
      val headOption = pq.ceil(Long.MinValue) // headOption method missing

      headOption.fold {
        infoVar.set(infInfo)(tx.peer)   // so that subsequent `schedule` will succeed

      } { case (newTargetTime, _) =>
        val t       = time
        val newInfo = new Info(issueTime = t, targetTime = newTargetTime)
        submit(newInfo)
      }
    }
  }
}