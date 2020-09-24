package de.sciss.synth.proc.tests

import de.sciss.file.File
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.synth.{InMemory, Server}
import de.sciss.lucre.{Cursor, Source, synth}
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Bounce, Durable, Proc, SoundProcesses, TimeRef, Timeline, Universe, showTransportLog}
import de.sciss.synth.{SynthGraph, ugen}
import org.rogach.scallop.{ScallopConf, ScallopOption => Opt}

import scala.concurrent.ExecutionContext

// XXX TODO: this should be a ScalaTest spec, opening the file after bouncing, and
// verifying the contents (easy with a sine).
object BounceTest {
  case class Config(realtime: Boolean = false, inMemory: Boolean = false)

  def main(args: Array[String]): Unit = {
    object p extends ScallopConf {
      printedName = "BounceTest"

      val realtime: Opt[Boolean] = opt(name = "realtime" )
      val inMemory: Opt[Boolean] = opt(name = "in-memory")

      verify()

      val config: Config = Config(realtime = realtime(), inMemory = inMemory())
    }

    import p.config._

    SoundProcesses.init()
    if (inMemory) {
      type S = InMemory
      implicit val system: S = InMemory()
      new BounceTest(/*system,*/ realtime = realtime)
    } else {
      type S = Durable
      implicit val system: S = Durable(BerkeleyDB.tmp())
      new BounceTest(/*system,*/ realtime = realtime)
    }
  }
}
class BounceTest[T <: synth.Txn[T]](/*val system: T,*/ realtime: Boolean)(implicit cursor: Cursor[T]) {
  de.sciss.lucre.synth.showLog = true
  showTransportLog  = !realtime

  def frame(secs: Double): Long = (secs * TimeRef.SampleRate).toLong

  println(
    """Expected outcome:
      |
      |A sound file of duration 150ms. a sine tone of 200 Hz
      |is seen for 50ms (or 10 periods), the remaining 100ms are silent.
      |
      |When using --realtime, the sound lasts 1s and the file has a duration of approx. 3s.
      |""".stripMargin)

  val groupH: Source[T, Timeline.Modifiable[T]] = cursor.step { implicit tx =>
//    val expr      = ExprImplicits[S]
//    import expr._
    // import ExprImplicits._

    val proc      = Proc[T]()
    proc.name     = "sinosc"
    proc.graph()  = SynthGraph {
      import ugen._
      val sig = SinOsc.ar(200)
      // sig.poll(5, "sig-out")
      Out.ar(0, sig)
    }
    val group     = Timeline[T]()
    group.add(Span(frame(if (realtime) 0.25 else 0.1), frame(if (realtime) 1.25 else 0.2)), proc)
    // import ProcGroup.serializer
    tx.newHandle(group)
  }

  // type I = InMemory

//  implicit val bridge: T => system.I#Tx = system.inMemoryTx

  implicit val u: Universe[T] = cursor.step { implicit tx => Universe.dummy }
  val bounce: Bounce[T]   = Bounce()
  val bCfg: Bounce.ConfigBuilder[T] = Bounce.Config()
  bCfg.group              = groupH :: Nil
  bCfg.span               = Span(frame(0.15), frame(if (realtime) 3.15 else 0.3)) // start in the middle of the proc span
  bCfg.realtime           = realtime
  val sCfg: Server.ConfigBuilder = bCfg.server
  //sCfg.nrtCommandPath = "/Users/hhrutz/Desktop/test.osc"
  // sCfg.nrtOutputPath      = "/tmp/test.aif"
  //sCfg.programPath    = "/Applications/SuperCollider_3.6.5/SuperCollider.app/Contents/Resources/scsynth"

  // this is default now:
  // sCfg.inputBusChannels   = 0
  sCfg.outputBusChannels  = 1
  sCfg.sampleRate         = 44100
  if (realtime) {
    sCfg.pickPort()
  }

  // this is default now:
  // sCfg.blockSize          = 1       // sample accurate placement of synths

  val process: Processor[File] with Processor.Prepared = bounce.apply(bCfg)
  import ExecutionContext.Implicits.global

  val t: Thread= new Thread {
    override def run(): Unit = {
      this.synchronized(this.wait())
      sys.exit(0)
    }
  }
  t.start()

  var lastProgress = 0
  process.addListener {
    case progress @ Processor.Progress(_, _) =>
      val p = progress.toInt
      while (lastProgress < p) {
        print('#')
        lastProgress += 2
      }

    case Processor.Result(_, res) =>
      println(s" $lastProgress%")
      println(res)
      t.synchronized(t.notifyAll())
  }
  process.start()
}