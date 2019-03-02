package de.sciss.synth.proc

import de.sciss.file._
import de.sciss.lucre.expr.DoubleObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Disposable, Folder, Obj}
import de.sciss.lucre.synth.InMemory
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.synth.io.AudioFile
import org.scalactic.source
import org.scalatest.{Assertion, FutureOutcome, Matchers, compatible, fixture}

import scala.collection.immutable.{Iterable => IIterable}
import scala.concurrent.duration._
import scala.concurrent.stm.Txn
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.Try

abstract class BounceSpec extends fixture.AsyncFlatSpec with Matchers {
  // ---- abstract ----

  def blockSize : Int     = 1
  def sampleRate: Double  = 44100

  // ---- fixture ----

  type S            = Durable
  type I            = InMemory
  type FixtureParam = Universe[Durable]

  SoundProcesses.init()

  final def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    import sys.process._
    val scOk = Try(Seq("scsynth", "-v").!!).getOrElse("").startsWith("scsynth ")
    if (scOk) {
      implicit val system: S = Durable(BerkeleyDB.tmp())
      val u: Universe[S] with Disposable[S#Tx] = system.step { implicit tx => Universe.dummy[S] }
      complete {
        test(u)
      } .lastly {
//        println("CLOSE")
        system.step { implicit tx =>
          u.dispose() // dispose the universe, because the Runner.Handler will otherwise be cached!
        }
        system.close()
      }
    } else {
      FutureOutcome.canceled("scsynth (SuperCollider) not found. Skipping test!")
    }
  }

  // ---- enrichment ----

  implicit final class NoCeremony(s: String) {
    def works(testFun: FixtureParam => Future[compatible.Assertion])(implicit pos: source.Position): Unit =
      s should "produce the predicted sound output" in testFun
  }

  implicit final class FrameOps(sec: Double) /* extends AnyVal */ {
    def seconds : Long = frame(sec)
    def secondsI: Int  = seconds.toInt

    def secondsFile : Long = (sec * sampleRate).toLong
    def secondsFileI: Int  = secondsFile.toInt
  }

  implicit class OutputOps(val `this`: Output[S]) /* extends AnyVal */ {
    def ~> (that: (Proc[S], String))(implicit tx: S#Tx): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).fold[Unit] {
        attr.put(key, `this`)
      } {
        case f: Folder[S] => f.addLast(`this`)
        case prev =>
          val f = Folder[S]
          f.addLast(prev)
          f.addLast(`this`)
          attr.put(key, f)
      }
    }

    def ~/> (that: (Proc[S], String))(implicit tx: S#Tx): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).getOrElse(sys.error(s"Attribute $key was not assigned")) match {
        case `sink` => attr.remove(key)
        case f: Folder[S] =>
          val idx = f.indexOf(`this`)
          if (idx < 0) sys.error(s"Attribute $key has a folder but does not contain ${`this`}")
          f.removeAt(idx)

        case other => sys.error(s"Cannot remove output from $other")
      }
    }
  }

  // ---- utility ----

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

  final def printVector(arr: Array[Float]): Unit = {
    val s = arr.grouped(12).map(_.mkString(", ")).mkString("Vector(", ",\n", ")")
    println(s) // arr.mkString("Vector(", ",", ")"))
  }

  final def mkSine(freq: Double, startFrame: Int, len: Int, sampleRate: Double = sampleRate): Array[Float] = {
    val freqN = 2 * math.Pi * freq / sampleRate
    Array.tabulate(len)(i => math.sin((startFrame + i) * freqN).toFloat)
  }

  final def mkLFPulse(freq: Double, startFrame: Int, len: Int, sampleRate: Double = sampleRate): Array[Float] = {
    val period = sampleRate / freq
    Array.tabulate(len)(i => if ((((startFrame + i) / period) % 1.0) < 0.5) 1f else 0f)
  }

  final def mulScalar(in: Array[Float], f: Float, start: Int = 0, len: Int = -1): Unit = {
    val len0 = if (len < 0) in.length - start else len
    var i = start
    val j = start + len0
    while (i < j) {
      in(i) *= f
      i += 1
    }
  }

  final def mulArray(a: Array[Float], b: Array[Float], start: Int = 0, len: Int = -1): Unit = {
    val len0 = if (len < 0) math.min(a.length, b.length) - start else len
    var i = start
    val j = start + len0
    while (i < j) {
      a(i) *= b(i)
      i += 1
    }
  }

  final def add(a: Array[Float], b: Array[Float], start: Int = 0, len: Int = -1): Unit = {
    val len0 = if (len < 0) math.min(a.length, b.length) - start else len
    var i = start
    val j = start + len0
    while (i < j) {
      a(i) += b(i)
      i += 1
    }
  }

  final def mkConstant(value: Float, len: Int): Array[Float] = Array.fill(len)(value)

  /** If `lineLen` is zero (default), it will be set to `len`. */
  final def mkLine(len: Int, start: Float = 0f, end: Float = 1f, startFrame: Int = 0, lineLen: Int = 0): Array[Float] = {
    val lineLen0 = if (lineLen == 0) len else lineLen
    import numbers.Implicits._
    Array.tabulate(len)(i => (i + startFrame).clip(0, lineLen0).linLin(0, lineLen0, start, end))
  }

  final def mkSilent(len: Int): Array[Float] = new Array(len)

  def doubleAttr(proc: Proc[S], key: String, value: Double)(implicit tx: S#Tx): Unit =
    proc.attr.put(key, value: DoubleObj[S])

  def addOutput(proc: Proc[S], key: String = "out")(implicit tx: S#Tx): Output[S] =
    proc.outputs.add(key)

  // ---- impl ----

  final def sampleRateI: Int = {
    val res = sampleRate.toInt
    require (res == sampleRate)
    res
  }

  final def assertSameSignal(a: Array[Float], b: Array[Float], tol: Float = 1.0e-4f): Assertion = {
    assert(a.length === b.length +- blockSize)
    val diff = (a, b).zipped.map((x, y) => math.abs(x - y))
    all (diff) should be < tol
  }

  def requireOutsideTxn(): Unit =
    require(Txn.findCurrent.isEmpty, "Must be called outside of transaction")

  object Objects {
    implicit def single  (x :           stm.Source[S#Tx, Obj[S]] ): Objects = new Objects(x :: Nil)
    implicit def multiple(xs: IIterable[stm.Source[S#Tx, Obj[S]]]): Objects = new Objects(xs)
  }
  final class Objects(val peer: IIterable[stm.Source[S#Tx, Obj[S]]])

  final def config(objects: Objects, span: Span, numChannels: Int = 1): Bounce.ConfigBuilder[S] = {
    requireOutsideTxn()
    val res = Bounce.Config[S]()
    res.span                      = span
    res.server.outputBusChannels  = numChannels
    res.server.sampleRate         = sampleRateI
    res.server.blockSize          = blockSize
    res.server.nrtOutputPath      = File.createTemp(suffix = ".aif").path
    res.group                     = objects.peer
    res
  }

  final def action(config: Bounce.ConfigBuilder[S], frame: Long)(fun: S#Tx => Unit): Unit =
    config.actions = config.actions ++ (Scheduler.Entry[S](time = frame, fun = fun) :: Nil)

  final def bounce(config: Bounce.Config[S], timeOut: Duration = 20.seconds)
                  (implicit universe: Universe[S]): Future[Array[Array[Float]]] = {
    requireOutsideTxn()
    val b = Bounce[S]()
    val p = b(config)
    // Important: don't use the single threaded SP context,
    // as bounce will block and hang
    import ExecutionContext.Implicits.global
    p.start()(global)
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
    } (global)
  }
}