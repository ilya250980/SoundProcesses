package de.sciss.proc

import de.sciss.lucre.Cursor
import de.sciss.lucre.synth.InMemory
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

trait InMemoryEventSpec extends FixtureAnyFlatSpec with Matchers {
  type S = InMemory
  type T = InMemory.Txn
  type FixtureParam = Cursor[T]

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
