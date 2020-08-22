package de.sciss
package synth
package proc

import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.stm.{Folder, Obj}
import de.sciss.span.Span
import de.sciss.synth.proc.Implicits._
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec
import org.scalatest.matchers.should.Matchers

/*
 To test only this suite:

 testOnly de.sciss.synth.proc.TimelineSerializationSpec

 */
class TimelineSerializationSpec extends FixtureAnyFlatSpec with Matchers {
  type S = Durable
  type FixtureParam = Durable

  SoundProcesses.init()

  final def withFixture(test: OneArgTest): Outcome = {
    val system = Durable(BerkeleyDB.tmp())
    try {
      test(system)
    }
    finally {
      system.close()
    }
  }

  "Timeline" should "serialize and deserialize" in { system =>
    val tH = system.step { implicit tx =>
      val t = Timeline[S]()
      val p = Proc[S]()
      p.name = "Schoko"
      assert(p.name === "Schoko")
      t.add(Span(0L, 10000L), p)
      t.name = "Britzel"
      tx.newHandle(t)
    }

    val oH = system.step { implicit tx =>
      val t = tH()  // uses direct serializer
      val objects = t.intersect(0L).toList.flatMap(_._2.map(_.value))
      assert(objects.map(_.name) === List("Schoko"))
      tx.newHandle(t: Obj[S])
    }

    system.step { implicit tx =>
      val o = oH()  // uses Obj serializer
      assert(o.name === "Britzel")
    }

    val fH = system.step { implicit tx =>
      val t = tH()
      val f = Folder[S]()
      f.addLast(t)
      tx.newHandle(f)
    }

    system.step { implicit tx =>
      val f = fH()
      val o = f.last
      assert(o.isInstanceOf[Timeline[S]])
      val t = o.asInstanceOf[Timeline[S]]
      val objects = t.intersect(0L).toList.flatMap(_._2.map(_.value))
      assert(objects.map(_.name) === List("Schoko"))
    }
  }
}
