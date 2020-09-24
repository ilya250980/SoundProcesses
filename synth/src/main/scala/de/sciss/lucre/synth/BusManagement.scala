/*
 *  BusManagement.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import de.sciss.lucre.Txn.{peer => itx}
import de.sciss.numbers
import de.sciss.synth.{AudioRated, ControlRated, Rate, AudioBus => SAudioBus, Bus => SBus, ControlBus => SControlBus}

import scala.collection.immutable.{SortedMap => ISortedMap}
import scala.concurrent.stm.{Ref, TMap, TSet}

sealed trait Bus {
  def server: Server
  def numChannels: Int
  def rate: Rate
}

object AudioBus {
  /** A consumer reading or writing from an audio bus.
    * Since a AudioBus is a meta structure, the
    * underlying audio bus may change due to optimization.
    * In this case the consumer is asked to update its
    * data. Also initial bus allocation is lazy, therefore
    * when adding the user as reader or writer, the
    * bus implementation will push its initial allocation
    * information to the user.
    */
  trait User /* extends Bus.User */ {
    def busChanged(peer: SAudioBus, isDummy: Boolean)(implicit tx: RT): Unit
  }
}

trait AudioBus extends Bus with AudioRated {
  import AudioBus._

  def busOption(implicit tx: RT): Option[SAudioBus]

  /** Adds a reading consumer to the bus. Note that
    * the readers are kept in a Set and this method doesn't
    * currently check whether the set already contains
    * the reader. Adding the same reader more than once
    * will cause malfunction.
    *
    * As a consequence, the user's busChanged method is
    * invoked with the current bus. The current bus may
    * change due to the addition. In this case, busChanged
    * is called on all other currently registered users.
    */
  def addReader(u: User)(implicit tx: RT): Unit

  /** Adds a writing consumer to the bus. Note that
    * the writers are kept in a Set and this method doesn't
    * currently check whether the set already contains
    * the writer. Adding the same writer more than once
    * will cause malfunction.
    *
    * As a consequence, the user's busChanged method is
    * invoked with the current bus. The current bus may
    * change due to the addition. In this case, busChanged
    * is called on all other currently registered users.
    */
  def addWriter(u: User)(implicit tx: RT): Unit

  /** Removes a reading consumer from the bus. It is
    * safe to call this method, passing in a user which
    * has already been previously removed.
    *
    * The current bus may change due to the removal.
    * In this case, busChanged is called on all
    * remaining registered users.
    */
  def removeReader(u: User)(implicit tx: RT): Unit

  /** Removes a writing consumer from the bus. It is
    * safe to call this method, passing in a user which
    * has already been previously removed.
    *
    * The current bus may change due to the removal.
    * In this case, busChanged is called on all
    * remaining registered users.
    */
  def removeWriter(u: User)(implicit tx: RT): Unit
}

object ControlBus {
  trait User /* extends Bus.User */ {
    def busChanged(bus: SControlBus)(implicit tx: RT): Unit
  }
}

trait ControlBus extends Bus with ControlRated {
  import ControlBus._

  def busOption(implicit tx: RT): Option[SControlBus]

  /** Adds a reading consumer to the bus. Note that
    * the readers are kept in a Set and this method doesn't
    * currently check whether the set already contains
    * the reader. Adding the same reader more than once
    * will cause malfunction.
    *
    * As a consequence, the user's busChanged method is
    * invoked with the current bus.
    */
  def addReader(u: User)(implicit tx: RT): Unit

  /** Adds a writing consumer to the bus. Note that
    * the writers are kept in a Set and this method doesn't
    * currently check whether the set already contains
    * the writer. Adding the same writer more than once
    * will cause malfunction.
    *
    * As a consequence, the user's busChanged method is
    * invoked with the current bus.
    */
  def addWriter(u: User)(implicit tx: RT): Unit

  /** Removes a reading consumer from the bus. It is
    * safe to call this method, passing in a user which
    * has already been previously removed.
    */
  def removeReader(u: User)(implicit tx: RT): Unit

  /** Removes a writing consumer from the bus. It is
    * safe to call this method, passing in a user which
    * has already been previously removed.
    */
  def removeWriter(u: User)(implicit tx: RT): Unit
}

object Bus {
  //   trait User {
  //      def busChanged( bus: Bus )( implicit tx: Txn ) : Unit
  //   }

  /** Constructs a new audio bus proxy for use in a shared environment, where
    * there can be situations of semi-orphaned buses (only one reader or
    * only one writer left).
    */
  def audio  (server: Server, numChannels: Int): AudioBus   = new AudioImpl  (server, numChannels)
  def control(server: Server, numChannels: Int): ControlBus = new ControlImpl(server, numChannels)

  /** Constructs a new audio bus proxy for use in a short-term temporary fashion.
    * The implementation does not maintain dummy and empty buses for the case that
    * there is only one reader or only one writer. As a consequence, it should not
    * be used in such a scenario, as precious bus indices will be occupied. On the
    * other hand, this method is useful for internal temporary buses, because when
    * both a reader and a writer release the resource, there are no spurious
    * bus re-assignments causing further busChanged notifications (which would go
    * to concurrently freed nodes).
    */
  def tmpAudio(server: Server, numChannels: Int): AudioBus = new TempAudioImpl(server, numChannels)

  def soundIn(server: Server, numChannels: Int, offset: Int = 0): AudioBus = {
    val o = server.peer.config
    require(offset + numChannels <= o.inputBusChannels, "soundIn - offset is beyond allocated hardware channels")
    FixedImpl(server, SAudioBus(server.peer, index = o.outputBusChannels + offset, numChannels = numChannels))
  }

  def soundOut(server: Server, numChannels: Int, offset: Int = 0): AudioBus = {
    val o = server.peer.config
    require(offset + numChannels <= o.outputBusChannels, "soundOut - offset is beyond allocated hardware channels")
    FixedImpl(server, SAudioBus(server.peer, index = offset, numChannels = numChannels))
  }

  def wrap(server: Server, bus: SAudioBus): AudioBus = {
    require(server.peer == bus.server)
    FixedImpl(server, bus)
  }

  // var verbose = false

  private sealed trait BusHolder[A <: SBus] {
    // ---- abstract ----

    def peer: A

    protected def remove()(implicit tx: RT): Unit

    protected def useCount: Ref[Int]

    // ---- impl ----

    // increments use count
    final def alloc()(implicit tx: RT): Unit = {
      useCount += 1
    }

    // decrements use count and calls `remove` if that count reaches zero
    final def free()(implicit tx: RT): Unit = {
      val cnt = useCount() - 1
      // if (verbose) println(s"$peer.free -> $cnt")
      require(cnt >= 0)
      useCount() = cnt
      if (cnt == 0) remove()
    }

//    final def index      : Int = peer.index
//    final def numChannels: Int = peer.numChannels
  }

  private type AudioBusHolder   = BusHolder[SAudioBus  ]
  private type ControlBusHolder = BusHolder[SControlBus]

  private final class PlainAudioBusHolder(server: Server, val peer: SAudioBus)
    extends BusHolder[SAudioBus] {

    protected val useCount: Ref[Int] = Ref(0)

    protected def remove()(implicit tx: RT): Unit =
      server.freeAudioBus(peer.index, peer.numChannels)
  }

  private final class PlainControlBusHolder(server: Server, val peer: SControlBus)
    extends BusHolder[SControlBus] {

    protected val useCount: Ref[Int] = Ref(0)

    protected def remove()(implicit tx: RT): Unit =
      server.freeControlBus(peer.index, peer.numChannels)
  }

  // full is the possibly re-used bus, it may have more channels than peer.
  // peer be either full or share full index but less channels.
  private final class OneWayAudioBusHolder(val server: Server, full: SAudioBus,
                                           val peer: SAudioBus, mapRef: ABusHolderMap, val useCount: Ref[Int])
    extends AudioBusHolder {

    if (full.index != peer.index || full.numChannels < peer.numChannels)
      throw new IllegalStateException(s"full = $full, peer = $peer")

    /** Returns a copy with `peer` having the desired number of channels. */
    def withChannels(n: Int): OneWayAudioBusHolder =
      if (peer.numChannels == n) this
      else new OneWayAudioBusHolder(server, full = full, peer = full.copy(numChannels = n), mapRef = mapRef,
                                            useCount = useCount)

    def add()(implicit tx: RT): Unit = {
      val m0 = mapRef.getOrElse(server, emptySortedMap)
      val m1 = m0 + ((full.numChannels, this))
      mapRef.put(server, m1)
    }

    protected def remove()(implicit tx: RT): Unit = {
      // println(s"---------FREE (${full.numChannels}) -> ${full.index}")
      server.freeAudioBus(full.index, full.numChannels)
      val m = mapRef(server) - full.numChannels
      if (m.isEmpty)
        mapRef.remove(server)
      else
        mapRef.put(server, m)
    }
  }

  // the number of channels key in the sorted map is always a power of two
  private type ABusHolderMap = TMap[Server, ISortedMap[Int, OneWayAudioBusHolder]]
  private val readOnlyBuses  : ABusHolderMap = TMap.empty
  private val writeOnlyBuses : ABusHolderMap = TMap.empty
  private val emptySortedMap = ISortedMap.empty[Int, OneWayAudioBusHolder]

  // XXX TODO - would be better if `Server` had a model that dispatches disposal
  private[synth] def serverRemoved(server: Server)(implicit tx: RT): Unit = {
    readOnlyBuses .remove(server)
    writeOnlyBuses.remove(server)
  }

  private def createReadOnlyBus(server: Server, numChannels: Int)(implicit tx: RT): AudioBusHolder =
    createOneWayAudioBus(server, numChannels, readOnlyBuses)

  private def createWriteOnlyBus(server: Server, numChannels: Int)(implicit tx: RT): AudioBusHolder =
    createOneWayAudioBus(server, numChannels, writeOnlyBuses)

  private def createOneWayAudioBus(server: Server, numChannels: Int, mapRef: ABusHolderMap)
                                (implicit tx: RT): AudioBusHolder = {
    val channelMap = mapRef.getOrElse(server, emptySortedMap)
    import numbers.Implicits._
    val n       = numChannels.nextPowerOfTwo
    channelMap.from(n).headOption.fold {
      val index = server.allocAudioBus(n)
      // println(s"---------ALLOC($n) -> $index")
      val full  = SAudioBus(server.peer, index = index, numChannels = n)
      val peer  = SAudioBus(server.peer, index = index, numChannels = numChannels)
      val res   = new OneWayAudioBusHolder(server, full = full, peer = peer, mapRef = mapRef, useCount = Ref(0))
      res.add()
      res
    } (_._2.withChannels(numChannels))
  }

  private def createAudioBus(server: Server, numChannels: Int)(implicit tx: RT): AudioBusHolder = {
    val index = server.allocAudioBus(numChannels)
    val peer  = SAudioBus(server.peer, index = index, numChannels = numChannels)
    new PlainAudioBusHolder(server, peer)
  }

  private def createControlBus(server: Server, numChannels: Int)(implicit tx: RT): ControlBusHolder = {
    val index = server.allocControlBus(numChannels)
    val peer  = SControlBus(server.peer, index = index, numChannels = numChannels)
    new PlainControlBusHolder(server, peer)
  }

  private abstract class AbstractAudioImpl extends AudioBus {
    import AudioBus.{User => AU}

    final protected val readers: TSet[AU] = TSet.empty
    final protected val writers: TSet[AU] = TSet.empty
  }

  private final case class FixedImpl(server: Server, bus: SAudioBus)
    extends AbstractAudioImpl {

    import AudioBus.{User => AU}

    def numChannels: Int = bus.numChannels

    def busOption(implicit tx: RT): Option[SAudioBus] = Some(bus)

    def addReader(u: AU)(implicit tx: RT): Unit = add(readers, u)
    def addWriter(u: AU)(implicit tx: RT): Unit = add(writers, u)

    private def add(users: TSet[AU], u: AU)(implicit tx: RT): Unit = {
      users.add(u)
      u.busChanged(bus, isDummy = false)
    }

    def removeReader(u: AU)(implicit tx: RT): Unit = remove(readers, u)
    def removeWriter(u: AU)(implicit tx: RT): Unit = remove(writers, u)

    private def remove(users: TSet[AU], u: AU)(implicit tx: RT): Unit =
      users.remove(u)

    override def toString = s"h-abus($bus)"
  }

  private abstract class BasicAudioImpl extends AbstractAudioImpl {
    final protected val bus: Ref[AudioBusHolder] = Ref.make[AudioBusHolder]()

    final def busOption(implicit tx: RT): Option[SAudioBus] = {
      val bh = bus()
      if (bh != null) Some(bh.peer) else None
    }
  }

  private final class AudioImpl(val server: Server, val numChannels: Int) extends BasicAudioImpl {
    import AudioBus.{User => AU}

    override def toString = s"sh-abus_$numChannels@${hashCode().toHexString}"

    def addReader(u: AU)(implicit tx: RT): Unit = {
      val rIsEmpty  = readers.isEmpty
      val wIsEmpty  = writers.isEmpty
      val bh = if (rIsEmpty) {
        if (wIsEmpty) {
          // no bus yet, create an empty shared one
          val res = createReadOnlyBus(server, numChannels)
          bus() = res
          res
        } else {
          // dispose old dummy bus, create new bus
          val res       = createAudioBus(server, numChannels)
          val newBus    = res.peer // AudioBus( server.peer, index = res.index, numChannels = numChannels )
          val oldHolder = bus.swap(res)
          readers.foreach { r =>
            oldHolder.free()
            r.busChanged(newBus, isDummy = false)
            res.alloc()
          }
          writers.foreach { w =>
            oldHolder.free()
            w.busChanged(newBus, isDummy = false)
            res.alloc()
          }
          res
        }
      } else {
        // re-use existing bus
        bus()
      }
      val isNew  = readers.add(u)
      if (!isNew) throw new IllegalArgumentException(s"Reading user $u was already added to $this")
      // always perform this on the newly added
      // reader no matter if the bus is new:
      bh.alloc()
      val newBus = bh.peer // AudioBus( server.peer, index = bh.index, numChannels = numChannels )
      u.busChanged(newBus, isDummy = wIsEmpty)
    }

    def addWriter(u: AU)(implicit tx: RT): Unit = {
      val rIsEmpty  = readers.isEmpty
      val wIsEmpty  = writers.isEmpty
      val bh = if (wIsEmpty) {
        if (rIsEmpty) {
          // no bus yet, create an empty shared one
          val res = createWriteOnlyBus(server, numChannels)
          bus() = res
          res
        } else {
          // dispose old dummy bus, create new bus
          val res       = createAudioBus(server, numChannels)
          val newBus    = res.peer // AudioBus( server.peer, index = res.index, numChannels = numChannels )
          val oldHolder = bus.swap(res)
          readers foreach { r =>
            oldHolder.free()
            r.busChanged(newBus, isDummy = false)
            res.alloc()
          }
          writers.foreach { w =>
            oldHolder.free()
            w.busChanged(newBus, isDummy = false)
            res.alloc()
          }
          res
        }
      } else {
        // re-use existing bus
        bus()
      }
      val isNew  = writers.add(u)
      if (!isNew) throw new IllegalArgumentException(s"Writing user $u was already added to $this")
      // always perform this on the newly added
      // reader no matter if the bus is new:
      bh.alloc()
      val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
      u.busChanged(newBus, isDummy = rIsEmpty)
    }

    def removeReader(u: AU)(implicit tx: RT): Unit = {
      if (!readers.remove(u)) return

      val oldHolder = bus()
      oldHolder.free()
      if (readers.isEmpty) {
        if (writers.isEmpty) {
          bus() = null
        } else {
          // they can all go to write only
          val bh = createWriteOnlyBus(server, numChannels)
          bus() = bh
          val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
          writers.foreach { w =>
            oldHolder.free()
            w.busChanged(newBus, isDummy = true)
            bh.alloc()
          }
        }
      }
    }

    def removeWriter(u: AU)(implicit tx: RT): Unit = {
      if (!writers.remove(u)) return
      val oldHolder = bus()
      oldHolder.free()
      if (writers.isEmpty) {
        if (readers.isEmpty) {
          bus() = null
        } else {
          // they can all go to write only
          val bh = createReadOnlyBus(server, numChannels)
          bus() = bh
          val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
          readers.foreach { r =>
            oldHolder.free()
            r.busChanged(newBus, isDummy = true)
            bh.alloc()
          }
        }
      }
    }
  }

  private final class TempAudioImpl(val server: Server, val numChannels: Int) extends BasicAudioImpl {
    import AudioBus.{User => AU}

    override def toString = s"tmp-abus_$numChannels@${hashCode().toHexString}"

    def addReader(u: AU)(implicit tx: RT): Unit = add(readers, writers, u)
    def addWriter(u: AU)(implicit tx: RT): Unit = add(writers, readers, u)

    private def add(users: TSet[AU], others: TSet[AU], u: AU)(implicit tx: RT): Unit = {
      // do _not_ check for null
      // because we might have a disposed
      // bus there, so we must make sure to
      // re-allocate a new bus each time
      // the users count goes to 1!
      val uIsEmpty = users.isEmpty
      val oIsEmpty = others.isEmpty

      val bh = if (uIsEmpty && oIsEmpty) {
        val res = createAudioBus(server, numChannels)
        bus() = res
        res
      } else {
        // re-use existing bus
        bus()
      }

      val isNew = users.add(u)
      if (!isNew) throw new IllegalArgumentException(s"User $u was already added to $this")

      // always perform this on the newly added
      // reader no matter if the bus is new:
      bh.alloc()
      val newBus = bh.peer // new AudioBus( server, bh.index, numChannels )
      u.busChanged(newBus, isDummy = oIsEmpty)
    }

    def removeReader(u: AU)(implicit tx: RT): Unit = remove(readers, writers, u)
    def removeWriter(u: AU)(implicit tx: RT): Unit = remove(writers, readers, u)

    private def remove(users: TSet[AU], others: TSet[AU], u: AU)(implicit tx: RT): Unit = {
      if (!users.remove(u)) return
      val bh = bus()
      if (users.isEmpty) {
        others.foreach { u1 =>
          u1.busChanged(bh.peer, isDummy = true)
        }
      }
      bh.free()
    }
  }

  private final class ControlImpl(val server: Server, val numChannels: Int) extends ControlBus {
    import ControlBus.{User => CU}

    private val bus     = Ref.make[ControlBusHolder]()
    private val readers = Ref(Set.empty[CU])
    private val writers = Ref(Set.empty[CU])

    override def toString = s"cbus_$numChannels@${hashCode().toHexString}"

    def busOption(implicit tx: RT): Option[SControlBus] = {
      val bh = bus()
      if (bh != null) Some(bh.peer) else None
    }

    def addReader(u: CU)(implicit tx: RT): Unit = add(readers, writers, u)
    def addWriter(u: CU)(implicit tx: RT): Unit = add(writers, readers, u)

    private def add(users: Ref[Set[CU]], others: Ref[Set[CU]], u: CU)(implicit tx: RT): Unit = {
      val us = users()
      require(!us.contains(u))
      // do _not_ check for null
      // because we might have a disposed
      // bus there, so we must make sure to
      // re-allocate a new bus each time
      // the users count goes to 1!
      val bh = if (us.isEmpty && others().isEmpty) {
        val res = createControlBus(server, numChannels)
        bus() = res
        res
      } else {
        // re-use existing bus
        bus()
      }
      users() = us + u
      // always perform this on the newly added
      // reader no matter if the bus is new:
      bh.alloc()
      val newBus = bh.peer // new ControlBus( server, bh.index, numChannels )
      u.busChanged(newBus)
    }

    def removeReader(u: CU)(implicit tx: RT): Unit = remove(readers, u)
    def removeWriter(u: CU)(implicit tx: RT): Unit = remove(writers, u)

    private def remove(users: Ref[Set[CU]], u: CU)(implicit tx: RT): Unit = {
      val rw = users()
      if (!rw.contains(u)) return
      users() = rw - u
      bus().free()
    }
  }
}