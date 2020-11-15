package de.sciss.proc.tests

import de.sciss.lucre.synth.InMemory
import de.sciss.proc.SoundProcesses

object TestAtomicFailure extends App {
  type S = InMemory
  type T = InMemory.Txn

  implicit val system: S = InMemory()

  SoundProcesses.atomic[T, Unit] { implicit tx =>
    sys.error("Failure in atomic")
  }

  SoundProcesses.step[T]("test") { implicit tx =>
    sys.error("Failure in step")
  }

  println("Here")
}
