package de.sciss.proc.tests

import de.sciss.file._
import de.sciss.lucre.synth.impl.ServerImpl
import de.sciss.lucre.synth.{InMemory, Server, Sys}
import de.sciss.lucre.{Artifact, ArtifactLocation, Cursor, Log, Source, synth}
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.audiofile.{AudioFile, AudioFileSpec}
import de.sciss.log.Level
import de.sciss.proc.Implicits._
import de.sciss.proc.{AudioCue, Bounce, Proc, SoundProcesses, TimeRef, Timeline, Universe}
import de.sciss.synth.{SynthGraph, ugen}

import scala.concurrent.ExecutionContext

object Issue73 {
  def main(args: Array[String]): Unit = {
    SoundProcesses.init()
    type S = InMemory
    implicit val system: S = InMemory()
    new Issue73(system)
    ()
  }
}
class Issue73[T <: synth.Txn[T]](val system: Sys)(implicit cursor: Cursor[T]) {
  def frame(secs: Double): Long = (secs * TimeRef.SampleRate).toLong

  val durBounceMin: Double        = 0.2 // 10.0
  val durBounceSec: Double        = durBounceMin * 60
//  val fIn         : File          = file("/data/projects/Maeanderungen/audio_work/edited/Einleitung_NC_T061.wav")
  val fIn         : File          = file("/data/projects/Maeanderungen/audio_work/edited/MT-13_MR_T107.wav")
  val specIn      : AudioFileSpec = AudioFile.readSpec(fIn)

  println(s"numFrames ${specIn.numFrames}; numChannels ${specIn.numChannels}") // numFrames 208338; numChannels 1

  val groupH: Source[T, Timeline.Modifiable[T]] = cursor.step { implicit tx =>
    val proc      = Proc[T]()
    proc.name     = "issue"
    proc.graph()  = SynthGraph {
      import de.sciss.numbers.Implicits._
      import de.sciss.synth.proc.graph._
      import ugen.{VDiskIn => _, _}
      val factor = XLine.ar(1.0/16, 1.0, durBounceSec)
      val in = VDiskIn.ar("in", speed = factor, interp = 2 /* 4 */, maxSpeed = 1.0)
      factor.roundTo(0.01).poll(30.0.reciprocal, "f")
      Out.ar(0, in)
    }
    val locIn = ArtifactLocation.newConst[T](fIn.parent.toURI)
    val artIn = Artifact(locIn, fIn.toURI)
    val cueIn = AudioCue.Obj(artIn, specIn, 0L, 1.0)
    proc.attr.put("in", cueIn)
    val group     = Timeline[T]()
    group.add(Span(frame(0.0), frame(durBounceSec)), proc)
    tx.newHandle(group)
  }

//  implicit val bridge: T => system.I#Tx = system.inMemoryTx

  implicit val u: Universe[T] = cursor.step { implicit tx => Universe.dummy }
  val bounce: Bounce[T]   = Bounce()
  val bCfg: Bounce.ConfigBuilder[T] = Bounce.Config()
  bCfg.group              = groupH :: Nil
  bCfg.span               = Span(frame(0.0), frame(durBounceSec))
  bCfg.realtime           = true
  val sCfg: Server.ConfigBuilder = bCfg.server

  sCfg.outputBusChannels  = specIn.numChannels
  sCfg.sampleRate         = 48000
  sCfg.pickPort()

  Log.synth.level = Level.Debug
//  //  showTransportLog  = false
  bCfg.beforePlay = { (_, s) => s.peer.dumpOSC() }
  ServerImpl.DEBUG = true

  val process: Processor[File] with Processor.Prepared = bounce.apply(bCfg)
  import ExecutionContext.Implicits.global

  val t: Thread = new Thread {
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
//      while (lastProgress < p) {
//        print('#')
//        lastProgress += 2
//      }
      if (lastProgress < p && (p % 10) == 0) {
        println(s" $p%")
        lastProgress = p
      }

    case Processor.Result(_, res) =>
      println(s" $lastProgress% done.")
      println(res)
      t.synchronized(t.notifyAll())
  }
  process.start()
}