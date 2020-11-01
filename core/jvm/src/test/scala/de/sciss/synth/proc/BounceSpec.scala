package de.sciss.synth.proc

import de.sciss.file._
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.synth.{InMemory, RT, Server}
import de.sciss.lucre.{Disposable, DoubleObj, Folder, Obj, Source}
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.SynthGraph
import de.sciss.audiofile.AudioFile
import de.sciss.audiofile.AudioFile.Frames
import org.scalactic.source
import org.scalatest.flatspec.FixtureAsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, FutureOutcome, compatible}

import scala.collection.immutable.{Iterable => IIterable}
import scala.concurrent.duration._
import scala.concurrent.stm.Txn
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.language.implicitConversions
import scala.util.Try

abstract class BounceSpec extends FixtureAsyncFlatSpec with Matchers {
  // ---- abstract ----

  def blockSize : Int     = 1
  def sampleRate: Double  = 44100

  // ---- fixture ----

  type S            = Durable
  type T            = Durable.Txn
  type I            = InMemory.Txn
  type FixtureParam = Universe[T]

  SoundProcesses.init()

  def isLinux: Boolean = {
    val n = sys.props("os.name")
    n.contains("Linux")
  }

//  def hasJack(): Boolean = {
//    import sys.process._
//    Try(Seq("jack", "--version").!!).getOrElse("").startsWith("jack")
//  }

  final def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    import sys.process._
    val scOk = Try(Seq("scsynth", "-v").!!).getOrElse("").startsWith("scsynth ")
    if (scOk) {
      implicit val system: S = Durable(BerkeleyDB.tmp())
      val u: Universe[T] with Disposable[T] = system.step { implicit tx => Universe.dummy[T] }
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

  implicit class OutputOps(val `this`: Proc.Output[T]) /* extends AnyVal */ {
    def ~> (that: (Proc[T], String))(implicit tx: T): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).fold[Unit] {
        attr.put(key, `this`)
        ()
      } {
        case f: Folder[T] => f.addLast(`this`)
        case prev =>
          val f = Folder[T]()
          f.addLast(prev)
          f.addLast(`this`)
          attr.put(key, f)
          ()
      }
    }

    def ~/> (that: (Proc[T], String))(implicit tx: T): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).getOrElse(sys.error(s"Attribute $key was not assigned")) match {
        case `sink` =>
          attr.remove(key)
          ()
        case f: Folder[T] =>
          val idx = f.indexOf(`this`)
          if (idx < 0) sys.error(s"Attribute $key has a folder but does not contain ${`this`}")
          f.removeAt(idx)
          ()

        case other => sys.error(s"Cannot remove output from $other")
      }
    }
  }

  // ---- utility ----

  def frame  (secs  : Double): Long   = (secs  * TimeRef.SampleRate).toLong
  def seconds(frames: Long  ): Double = frames / TimeRef.SampleRate

  def proc(graph: => Any)(implicit tx: T): Proc[T] = {
    val p = Proc[T]()
    val g = SynthGraph {
      graph
      ()
    }
    p.graph() = SynthGraphObj.newConst[T](g)
    p
  }

  final def printVector(arr: Array[Float]): Unit = {
    val s = arr.grouped(12).map(_.mkString(", ")).mkString("Vector(", ",\n", ")")
    println(s) // arr.mkString("Vector(", ",", ")"))
  }

  final def mkDC(len: Int, amp: Double = 1.0): Array[Float] = {
    val ampF = amp.toFloat
    Array.fill(len)(ampF)
  }

  final def mkSine(freq: Double, startFrame: Int, len: Int, sampleRate: Double = sampleRate,
                   amp: Double = 1.0): Array[Float] = {
    val freqN = 2 * math.Pi * freq / sampleRate
    Array.tabulate(len)(i => (math.sin((startFrame + i) * freqN) * amp).toFloat)
  }

  final def mkLFPulse(freq: Double, startFrame: Int, len: Int, sampleRate: Double = sampleRate,
                      amp: Double = 1.0): Array[Float] = {
    val period = sampleRate / freq
    Array.tabulate(len)(i => if ((((startFrame + i) / period) % 1.0) < 0.5) amp.toFloat else 0f)
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

  /** Adds `b` to `a` */
  final def add(a: Array[Float], b: Array[Float], aOff: Int = 0, bOff: Int = 0, len: Int = -1): Unit = {
    val len0 = if (len < 0) math.min(a.length - aOff, b.length - bOff) else len
    var i = aOff
    val j = aOff + len0
    var k = bOff
    while (i < j) {
      a(i) += b(k)
      i += 1
      k += 1
    }
  }

  final def mkConstant(value: Float, len: Int): Array[Float] = Array.fill(len)(value)

  /** If `lineLen` is zero (default), it will be set to `len`. */
  final def mkLine(len: Int, start: Float = 0f, end: Float = 1f, startFrame: Int = 0, lineLen: Int = 0): Array[Float] = {
    val lineLen0 = if (lineLen == 0) len else lineLen
    import numbers.Implicits._
    Array.tabulate(len)(i => (i + startFrame).clip(0, lineLen0).linLin(0, lineLen0.toFloat, start, end))
  }

  final def mkSilent(len: Int): Array[Float] = new Array(len)

  def doubleAttr(proc: Proc[T], key: String, value: Double)(implicit tx: T): Unit = {
    proc.attr.put(key, value: DoubleObj[T])
    ()
  }

  def addOutput(proc: Proc[T], key: String = "out")(implicit tx: T): Proc.Output[T] =
    proc.outputs.add(key)

  // ---- impl ----

  final def sampleRateI: Int = {
    val res = sampleRate.toInt
    require (res == sampleRate)
    res
  }

  final def assertSameSignal(a: Array[Float], b: Array[Float], tol: Float = 1.0e-4f,
                             lengthTol: Int = blockSize, dropOuts: Int = 0): Assertion = {
    assert(a.length === b.length +- lengthTol)
    val diff = (a.iterator zip b.iterator).map { case (x, y) => math.abs(x - y) } .toIndexedSeq
    atLeast (diff.length - dropOuts, diff) should be < tol
  }

  def requireOutsideTxn(): Unit =
    require(Txn.findCurrent.isEmpty, "Must be called outside of transaction")

  object Objects {
    implicit def single  (x :           Source[T, Obj[T]] ): Objects = new Objects(x :: Nil)
    implicit def multiple(xs: IIterable[Source[T, Obj[T]]]): Objects = new Objects(xs)
  }
  final class Objects(val peer: IIterable[Source[T, Obj[T]]])

  final def config(objects: Objects, span: Span, numChannels: Int = 1): Bounce.ConfigBuilder[T] = {
    requireOutsideTxn()
    val res = Bounce.Config[T]()
    res.span                      = span
    res.server.outputBusChannels  = numChannels
    res.server.sampleRate         = sampleRateI
    res.server.blockSize          = blockSize
    res.server.nrtOutputPath      = File.createTemp(suffix = ".aif").path
    res.group                     = objects.peer
    res
  }

  final def action(config: Bounce.ConfigBuilder[T], frame: Long)(fun: T => Unit): Unit =
    config.actions = config.actions ++ (Scheduler.Entry[T](time = frame, fun = fun) :: Nil)

  final def runServer[A](config: Server.ConfigBuilder = Server.Config())(fun: Server => Future[A])
                     (implicit universe: FixtureParam): Future[A] = {
    import universe.cursor
    val p = Promise[A]()
    cursor.step { implicit tx =>
      val jackOption = mkJack(config)

      universe.auralSystem.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit itx: RT): Unit = itx.afterCommit {
          p.completeWith(fun(s).andThen { case _ =>
            jackOption.foreach(_.destroy())
          })
        }

        def auralStopped()(implicit tx: RT): Unit = ()
      })

      universe.auralSystem.start(config)
      p.future
    }
  }

  def jackName: String = {
    getClass.getSimpleName.filter(_.isLetterOrDigit)
  }

  private def mkJack(config: Server.ConfigBuilder): Option[Process] = {
    if (!isLinux) None else {
      val sr0       = config.sampleRate
      val sr        = if (sr0 == 0) sampleRate else sr0
      val n         = jackName
      config.deviceName = Some(s"$n:scsynth")
      val args      = List("jackd", "-n", n, "-d", "dummy", "-r", sr.toString)
      val pb        = new ProcessBuilder(args: _*)
      val res       = pb.start()
      Some(res)
    }
  }

  final def bounce(config: Bounce.ConfigBuilder[T], timeOut: Duration = 20.seconds, debugKeep: Boolean = false)
                  (implicit universe: Universe[T]): Future[Array[Array[Float]]] = {
    requireOutsideTxn()
    val jackOption = mkJack(config.server)
    val b = Bounce[T]()

    val processor = b(config)
    // Important: don't use the single threaded SP context,
    // as bounce will block and hang
    import ExecutionContext.Implicits.global
    processor.start()(global)
    val fDone: Future[Frames] = processor.map { f =>
      if (debugKeep) println(s"Bounce file: $f")
      try {
        val a = AudioFile.openRead(f)
        // println(s"NUM-FRAMES = ${a.numFrames}; FILE-LENGTh =  ${f.length()}")
        require(a.numFrames < 0x7FFFFFFF)
        val buf = a.buffer(a.numFrames.toInt)
        a.read(buf)
        a.close()
        buf
      } finally {
        if (!debugKeep) {
          f.delete()
          ()
        }
      }
    } (global)

    fDone.andThen { case _ =>
      jackOption.foreach { process =>
        process.destroy()
        // tricky: if we run multiple tests,
        // we may otherwise run into
        // '`unit_tests' server already active'
        Thread.sleep(1000)
      }
    } (global)
  }
}