package de.sciss

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc.{Confluent, SoundProcesses}
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.TxnLocal

trait ConfluentEventSpec extends FixtureAnyFlatSpec with Matchers {
  type S = Confluent
  type D = S#D
  type FixtureParam = lucre.confluent.Cursor[S, D]

  SoundProcesses.init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Confluent(BerkeleyDB.tmp())
    try {
      val (_, cursor) = system.cursorRoot(_ => ())(implicit tx => _ => system.newCursor())
      test(cursor)
    }
    finally {
      system.close()
    }
  }

  final class Observation {
    private val seqRef = TxnLocal(init = Vec.empty[Any])

    def register(tx: S#Tx)(upd: Any): Unit = {
      seqRef.transform(_ :+ upd)(tx.peer)
    }

    def assertEquals(expected: Any*)(implicit tx: S#Tx): Unit = {
      val ob = seqRef.get(tx.peer)
      assert(ob === expected.toIndexedSeq, "Expected\n   " + expected.mkString("[", ", ", "]")
        + "\n...but observed\n   " + ob.mkString("[", ", ", "]"))
    }

    def clear()(implicit tx: S#Tx): Unit = {
      seqRef.set(Vector.empty)(tx.peer)
    }

    def assertEmpty()(implicit tx: S#Tx): Unit = {
      assertEquals()
    }

    def print()(implicit tx: S#Tx): Unit = {
      println(seqRef.get(tx.peer).mkString("[", ", ", "]"))
    }
  }
}
