package de.sciss.synth.proc.tests

import de.sciss.lucre.synth.{AudioBus, Bus, InMemory, RT}
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.{AudioBus => SAudioBus}

// XXX TODO --- make this a ScalaTest instance
object BusTest extends App {
  val cursor  = InMemory()
  val as      = AuralSystem()

  class DummyUser extends AudioBus.User {
    var observed = Vector.empty[Boolean]

    def busChanged(peer: SAudioBus, isDummy: Boolean)(implicit tx: RT): Unit = observed :+= isDummy
  }

  val sync = new AnyRef
  new Thread {
    override def run(): Unit = {
      sync.synchronized(sync.wait())
      sys.exit()
    }
    start()
  }

  cursor.step { implicit tx =>
    as.whenStarted { s =>
      cursor.step { implicit tx =>
        val bus = Bus.audio(s, 2)
        assert(bus.busOption.isEmpty)
        val user = new DummyUser

        def assertObserved(values: Boolean*): Unit =
          assert(user.observed == values.toVector, user.observed.mkString("[", ",", "]"))

        bus.addReader(user)
        assertObserved(true)
        assert(bus.busOption.isDefined)
        bus.addWriter(user)
        assertObserved(true, false, false)
        assert(bus.busOption.isDefined)
        bus.removeReader(user)
        assertObserved(true, false, false, true)
        assert(bus.busOption.isDefined)
        bus.removeWriter(user)
        assertObserved(true, false, false, true)
        assert(bus.busOption.isEmpty)
      }
      sync.synchronized(sync.notify())
    }
    as.start()
  }
}