package de.sciss.synth.proc.tests

import de.sciss.log.Level
import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.SoundProcesses.logTransport
import de.sciss.synth.proc.{Scheduler, TimeRef}

// TODO - could be rather easily converted into a unit test
object SchedulerTest extends App {
  type S = InMemory
  type T = InMemory.Txn

  implicit val cursor: S = InMemory()
  logTransport.level  = Level.Debug

  def frames(secs: Double) = (TimeRef.SampleRate * secs).toLong

  /*
      Expected output:

        After 1 seconds
        Logically 14112000 frames elapsed.

        After 3   seconds (txn hash <X>)
        After 1+2 seconds (txn hash <X>)
        Logically 28224000 frames elapsed.

        After 4 seconds
        Terminating.

        (the two hashes <X> must be the same; two 2-second item is omitted)
   */

  cursor.step { implicit tx =>
    val sched = Scheduler[T]()
    val now   = sched.time
    val token2 = sched.schedule(now + frames(2.0)) { implicit tx =>
      println("After 2 seconds - should have been cancelled")
    }
    sched.schedule(now + frames(4.0)) { implicit tx =>
      println("After 4 seconds")
    }
    sched.schedule(now + frames(1.0)) { implicit tx =>
      println("After 1 seconds")
      val now1 = sched.time
      println(s"Logically ${now1 - now} frames elapsed.")
      sched.schedule(now1 + frames(2.0)) { implicit tx =>
        println(s"After 1+2 seconds (txn hash ${tx.hashCode().toHexString})")
        val now2 = sched.time
        println(s"Logically ${now2 - now1} frames elapsed.")
      }
      ()
    }
    sched.schedule(now + frames(3.0)) { implicit tx =>
      println(s"After 3   seconds (txn hash ${tx.hashCode().toHexString})")
    }
    sched.cancel(token2)
  }

  new Thread {
    override def run(): Unit = {
      Thread.sleep(5000)
      println("Terminating.")
      sys.exit()
    }
    start()
  }
}
