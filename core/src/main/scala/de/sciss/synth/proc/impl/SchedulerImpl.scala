/*
 *  SchedulerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import java.util.concurrent.TimeUnit

import de.sciss.lucre.data.SkipList
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.serial.ImmutableSerializer
import de.sciss.synth.proc
import de.sciss.synth.proc.Scheduler.Entry
import proc.{logTransport => logT}

import scala.concurrent.stm.{InTxn, Ref, TMap, TxnLocal}
import scala.util.control.NonFatal

object SchedulerImpl {
  def apply[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Scheduler[S] = {
    val system  = tx.system
    val prio    = mkPrio[S, system.I](system)
    implicit val iSys: S#Tx => system.I#Tx = system.inMemoryTx
    new RealtimeImpl[S, system.I](prio)
  }

  def offline[S <: Sys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Scheduler.Offline[S] = {
    val system  = tx.system
    val prio    = mkPrio[S, system.I](system)
    implicit val iSys: S#Tx => system.I#Tx = system.inMemoryTx
    new OfflineImpl[S, system.I](prio)
  }

  // what a mess, Scala
  private def mkPrio[S <: Sys[S], I1 <: Sys[I1]](system: S { type I = I1 })
                                                (implicit tx: S#Tx): SkipList.Map[I1, Long, Set[Int]] = {
    implicit val iSys   : S#Tx => I1#Tx = system.inMemoryTx _
    implicit val itx    : I1#Tx         = iSys(tx)
    implicit val setSer: ImmutableSerializer[Set[Int]] = ImmutableSerializer.set
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

    import TimeRef.{framesAndSecs => fas}
    override def toString = s"[issueTime = ${fas(issueTime)}, targetTime = ${fas(targetTime)}]"
  }

  private val infInfo = new Info(issueTime = 0L, targetTime = Long.MaxValue)

  private final class OfflineImpl[S <: Sys[S], I <: stm.Sys[I]](protected val prio: SkipList.Map [I , Long, Set[Int]])
                                                                (implicit val cursor: stm.Cursor[S],
                                                                 protected val iSys: S#Tx => I#Tx)
    extends Impl[S, I] with Scheduler.Offline[S] {

    private val timeRef = Ref(0L)

    def           time               (implicit tx: S#Tx): Long = timeRef.get(tx.peer)
    protected def time_=(value: Long)(implicit tx: S#Tx): Unit = timeRef.set(value)(tx.peer)

//    def systemTimeNanos(implicit tx: S#Tx): Long = calcTimeNanos(time)

//    private def calcTimeNanos(frames: Long): Long = {
//      val framesNanos = (frames / sampleRateN).toLong
//      framesNanos
//    }

    def stepTag[A](fun: S#Tx => A): A = cursor.step(fun)

    protected def submit(info: Info)(implicit tx: S#Tx): Unit =
      infoVar.set(info)(tx.peer)

    def step()(implicit tx: S#Tx): Unit = {
      val info = infoVar.get(tx.peer)
      if (!info.isInf) eventReached(info)
    }

    def stepTarget(implicit tx: S#Tx): Option[Long] = {
      implicit val ptx: InTxn = tx.peer
      val info = infoVar()
      if (info.isInf) None else Some(info.targetTime)
    }
  }

  private final class RealtimeImpl[S <: Sys[S], I <: stm.Sys[I]](protected val prio: SkipList.Map [I , Long, Set[Int]])
                                                                (implicit val cursor: stm.Cursor[S],
                                                                 protected val iSys: S#Tx => I#Tx)
    extends Impl[S, I] {

    /*
      Timing information:

      - we can't get nanos and milliseconds atomically at the same time (can we?), therefore
        set nanos to zero for the absolute system time reference `timeAbsNanos`
      - the relative nanos are used to calculate frames in `calcFrame`
      - the `timeRef` freezes the advanced frames since creation time in sample frames
      - to convert this to system time, divide `timeRef` by the sample rate, multiply by 1.0e9,
        and add the absolute time reference.

     */

    private[this] val timeAbsNanos    = System.currentTimeMillis() * 1000000L
    private[this] val timeRelNanos    = System.nanoTime()
    private[this] val timeRef         = TxnLocal(calcFrame())

    def           time               (implicit tx: S#Tx): Long = timeRef.get(tx.peer)
    protected def time_=(value: Long)(implicit tx: S#Tx): Unit = timeRef.set(value)(tx.peer)

//    private def systemTimeNanos(implicit tx: S#Tx): Long = calcTimeNanos(time)

    private def calcTimeNanos(frames: Long): Long = {
      val framesNanos = (frames / sampleRateN).toLong
      timeAbsNanos + framesNanos
    }

    private def calcFrame(): Long = {
      // 1 ns = 10^-9 s
      val delta = System.nanoTime() - timeRelNanos
      (delta * sampleRateN).toLong
    }

    def stepTag[A](fun: S#Tx => A): A = {
      val nowFrames = calcFrame()
      val nowNanos  = calcTimeNanos(nowFrames)
      cursor.stepTag(nowNanos) { implicit tx =>
        time = nowFrames
        fun(tx)
      }
    }

    protected def submit(info: Info)(implicit tx: S#Tx): Unit = {
      implicit val ptx: InTxn = tx.peer
      infoVar()         = info
      val jitter        = calcFrame() - info.issueTime
      val actualDelayN  = math.max(0L, ((info.delay - jitter) / sampleRateN).toLong)
      logT(f"scheduled:     $info; log dly = ${TimeRef.framesAndSecs(info.delay)}, act dly = ${actualDelayN * 1.0e-9}%1.3fs")
      tx.afterCommit {
        SoundProcesses.scheduledExecutorService.schedule(new Runnable {
          def run(): Unit = {
            logT(s"scheduled: exe $info")
            val nowNanos = calcTimeNanos(info.targetTime)
            cursor.stepTag(nowNanos) { implicit tx =>
              eventReached(info)    // this calls `time_=(info.targetTime)`
            }
          }
        }, actualDelayN, TimeUnit.NANOSECONDS)
      }
    }
  }

  // one can argue whether the values should be ordered, e.g. Seq[Int] instead of Set[Int],
  // such that if two functions A and B are submitted after another for the same target time,
  // then A would be executed before B. But currently we don't think this is an important aspect.
  private abstract class Impl[S <: Sys[S], I <: stm.Sys[I]]
    extends Scheduler[S] {

    // ---- abstract ----

    protected def prio: SkipList.Map[I , Long, Set[Int]]
    protected def iSys: S#Tx => I#Tx

    /** Invoked to submit a schedule step either to a realtime scheduler or other mechanism.
      * When the step is performed, execution should be handed over to `eventReached`, passing
      * over the same three arguments.
      */
    protected def submit(info: Info)(implicit tx: S#Tx): Unit

    protected def time_=(value: Long)(implicit tx: S#Tx): Unit

    // ---- implemented ----

    private type Token = Int

    private val tokenRef    = Ref(0)
    private val tokenMap    = TMap.empty[Int, Entry[S]]

    final protected val sampleRateN = 0.014112 // = Timeline.SampleRate * 1.0e-9
    protected final val infoVar     = Ref(infInfo)

    // ---- scheduling ----

    final def schedule(targetTime: Long)(fun: S#Tx => Unit)(implicit tx: S#Tx): Token = {
      implicit val ptx: InTxn = tx.peer
      implicit val itx: I#Tx = iSys(tx)
      val t             = time
      if (targetTime < t) throw new IllegalArgumentException(s"Cannot schedule in the past ($targetTime < $time)")
      val token         = tokenRef.getAndTransform(_ + 1)
      tokenMap.put(token, new Entry[S](targetTime, fun))
      val oldInfo       = infoVar()
      val reschedule    = targetTime < oldInfo.targetTime

      if (reschedule) {   // implies that prio does not have an entry at `timeClip` yet
        // println(s"............... SCHEDULE t = $targetTime; token = $token - REPLACE")
        assert(!prio.contains(targetTime))
        prio.put(targetTime, Set(token))
      } else {
        val newSet = prio.get(targetTime).fold(Set(token))(_ + token)
        // println(s"............... SCHEDULE t = $targetTime; token = $token - ADD > $newSet")
        prio.put(targetTime, newSet)
      }

      import TimeRef.{framesAndSecs => fas}
      logT(s"schedule: token = $token, time = ${fas(t)}, old tgt ${fas(oldInfo.targetTime)}, new tgt = ${fas(targetTime)}, submit? $reschedule")

      if (reschedule) {
        val newInfo = new Info(issueTime = t, targetTime = targetTime)
        submit(newInfo)
      }

      token
    }

    final def cancel(token: Token)(implicit tx: S#Tx): Unit = {
      implicit val ptx: InTxn = tx.peer
      implicit val itx: I#Tx = iSys(tx)
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
        prio.get(t).foreach { set0 =>
          val set1  = set0 - token
          if (set1.isEmpty) {
            // println(s"............... .... > EMPTY")
            prio.remove(t)
            // if entry became empty, see if it was
            // scheduled; if so, re-submit
            val info = infoVar()
            if (info.targetTime == t) scheduleNext()

          } else {
            // println(s"............... .... > $set1")
            prio.put(t, set1)
          }
        }
      }
    }

    /** Invoked from the `submit` body after the scheduled event is reached. */
    final protected def eventReached(info: Info)(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx  = iSys(tx)
      implicit val ptx: InTxn = tx.peer
      if (info != infoVar()) return // the scheduled task was invalidated by an intermediate stop or seek command

      // this is crucial to eliminate drift: since we reached the scheduled event, do not
      // let the timeRef txn-local determine a new free wheeling time, but set it to the
      // time we targeted at; then in the next scheduleNext call, the jitter is properly
      // calculated.
      val t = info.targetTime
      time = t

      prio.remove(t).foreach { tokens =>
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
    private def scheduleNext()(implicit tx: S#Tx): Unit = {
      implicit val itx: I#Tx = iSys(tx)
      val headOption = prio.ceil(Long.MinValue) // headOption method missing

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