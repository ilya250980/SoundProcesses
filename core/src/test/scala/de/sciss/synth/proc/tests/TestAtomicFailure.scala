package de.sciss.synth.proc.tests

import de.sciss.lucre.synth.InMemory
import de.sciss.synth.proc.SoundProcesses

object TestAtomicFailure extends App {
  implicit val system: S = InMemory()

  type S = InMemory

  SoundProcesses.atomic[S, Unit] { implicit tx =>
    sys.error("Failure in atomic")
  }

  SoundProcesses.step[S]("test") { implicit tx =>
    sys.error("Failure in step")
  }

  println("Here")
}
