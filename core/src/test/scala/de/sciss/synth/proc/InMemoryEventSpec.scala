package de.sciss.synth.proc

import de.sciss.lucre.stm
import de.sciss.lucre.synth.InMemory
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait InMemoryEventSpec extends FixtureAnyFlatSpec with Matchers {
  type S = InMemory
  type FixtureParam = stm.Cursor[S]

  SoundProcesses.init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = InMemory()
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }
}
