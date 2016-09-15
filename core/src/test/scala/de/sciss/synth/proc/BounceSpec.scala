package de.sciss.synth.proc

import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.InMemory
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import org.scalatest.{FutureOutcome, Matchers, fixture}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.concurrent.stm.Txn
import scala.util.Try

abstract class BounceSpec extends fixture.AsyncFlatSpec with Matchers {
  // ---- abstract ----

  def blockSize : Int     = 1
  def sampleRate: Double  = 44100

  // ---- fixture ----

  type S            = Durable
  type I            = InMemory
  type FixtureParam = Durable

  SoundProcesses.init()

  final def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    import sys.process._
    val scOk = Try(Seq("scsynth", "-v").!!).getOrElse("").startsWith("scsynth ")
    if (scOk) {
      val system = Durable(BerkeleyDB.tmp())
      complete {
        test(system)
      } .lastly {
        system.close()
      }
    } else {
      FutureOutcome.canceled("scsynth (SuperCollider) not found. Skipping test!")
    }
  }

  // ---- utility ----

  implicit final class FrameOps(sec: Double) /* extends AnyVal */ {
    def seconds: Long = frame(sec)
  }

  def frame  (secs  : Double): Long   = (secs  * TimeRef.SampleRate).toLong
  def seconds(frames: Long  ): Double = frames / TimeRef.SampleRate

  def proc(graph: => Unit)(implicit tx: S#Tx): Proc[S] = {
    val p = Proc[S]
    val g = SynthGraph {
      graph
    }
    p.graph() = SynthGraphObj.newConst[S](g)
    p
  }

  final def printVector(arr: Array[Float]): Unit =
    println(arr.mkString("Vector(", ",", ")"))

  final def mkSine(freq: Double, startFrame: Int, len: Int, sampleRate: Double = sampleRate): Array[Float] = {
    val freqN = 2 * math.Pi * freq / sampleRate
    Array.tabulate(len)(i => math.sin((startFrame + i) * freqN).toFloat)
  }

  final def mkConstant(value: Float, len: Int): Array[Float] = Array.fill(len)(value)

  /** If `lineLen` is zero (default), it will be set to `len`. */
  final def mkLine(len: Int, start: Float = 0f, end: Float = 1f, startFrame: Int = 0, lineLen: Int = 0): Array[Float] = {
    val lineLen0 = if (lineLen == 0) len else lineLen
    import numbers.Implicits._
    Array.tabulate(len)(i => (i + startFrame).clip(0, lineLen0).linlin(0, lineLen0, start, end))
  }

  final def mkSilent(len: Int): Array[Float] = new Array(len)

  // ---- impl ----

  final def sampleRateI: Int = {
    val res = sampleRate.toInt
    require (res == sampleRate)
    res
  }

  final def assertSameSignal(a: Array[Float], b: Array[Float], tol: Float = 1.0e-4f) = {
    assert(a.length === b.length +- blockSize)
    val diff = (a, b).zipped.map((x, y) => math.abs(x - y))
    all (diff) should be < tol
  }

  def requireOutsideTxn(): Unit =
    require(Txn.findCurrent.isEmpty, "Must be called outside of transaction")

  final def config(obj: stm.Source[S#Tx, Obj[S]], span: Span, numChannels: Int = 1): Bounce.ConfigBuilder[S] = {
    requireOutsideTxn()
    val res = Bounce.Config[S]
    res.span                      = span
    res.server.outputBusChannels  = numChannels
    res.server.sampleRate         = sampleRateI
    res.server.blockSize          = blockSize
    res.server.nrtOutputPath      = File.createTemp(suffix = ".aif").path
    res.group                     = obj :: Nil
    res
  }

  final def bounce(config: Bounce.Config[S], timeOut: Duration = 20.seconds)
                  (implicit cursor: stm.Cursor[S]): Future[Array[Array[Float]]] = {
    requireOutsideTxn()
    import WorkspaceHandle.Implicits.dummy
    val b = Bounce[S, I]
    val p = b(config)
    // Important: don't use the single threaded SP context,
    // as bounce will block and hang
    p.start()(ExecutionContext.Implicits.global)
    p.map { f =>
      try {
        val a = AudioFile.openRead(f)
        require(a.numFrames < 0x7FFFFFFF)
        val buf = a.buffer(a.numFrames.toInt)
        a.read(buf)
        a.close()
        buf
      } finally {
        f.delete()
      }
    }
  }
}