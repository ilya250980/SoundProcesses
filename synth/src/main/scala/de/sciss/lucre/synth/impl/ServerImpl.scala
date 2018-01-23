/*
 *  ServerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package impl

import java.io.{ByteArrayOutputStream, DataOutputStream}

import de.sciss.lucre.stm.TxnLike.{peer => txPeer}
import de.sciss.osc
import de.sciss.osc.Timetag
import de.sciss.synth.{AllocatorExhausted, ControlABusMap, ControlSet, UGenGraph, addToHead, message, Client => SClient, Server => SServer}
import de.sciss.topology.Topology

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.mutable
import scala.concurrent.stm.{Ref, TMap}
import scala.concurrent.{ExecutionContext, Future, Promise}

object ServerImpl {
  def apply  (peer: SServer): Server          = OnlineImpl (peer)
  def offline(peer: SServer): Server.Offline  = OfflineImpl(peer)

  /** If `true`, checks against bundle size overflow (64K) and prints the bundle before crashing. */
  var VERIFY_BUNDLE_SIZE  = true
  /** If `true`, applies a few optimizations to messages within a bundle, in order to reduce its size */
  var USE_COMPRESSION     = true
  /** If `true`, check that wire-buffers are not exceeded before sending synth def */
  var VERIFY_WIRE_BUFFERS = true
  /** If `true` debug sending out stuff */
  var DEBUG               = false

  private final val MaxOnlinePacketSize   = 0x8000 // 0x10000 // 64K
  private final val MaxOfflinePacketSize  = 0x2000 // 8192

  def reduceFutures(futures: Vec[Future[Unit]])(implicit executionContext: ExecutionContext): Future[Unit] =
    futures match {
      case Vec()        => Future.successful(())
      case Vec(single)  => single
      case _ /* more */ => Future.reduce(futures)((_, _) => ()) // reduceLeft requires Scala 2.12
    }

  private final case class OnlineImpl(peer: SServer) extends Impl {
    override def toString: String = peer.toString

    def maxPacketSize: Int      = MaxOnlinePacketSize
    def isLocal      : Boolean  = peer.isLocal
    def isRealtime   : Boolean  = true

    // ---- compression ----

    private def compressControlSet(old: Seq[ControlSet], add: Seq[ControlSet]): Seq[ControlSet] = {
      var res = old
      add.foreach {
        case c @ ControlSet.Value(key, _) =>
          val j = res.indexWhere {
            case ControlSet.Value(`key`, _) => true
            case _ => false
          }
          res = if (j < 0) res :+ c else res.updated(j, c)

        case c => res :+= c
      }
      res
    }

    private def compress(b: osc.Bundle): osc.Bundle = {
      val in  = b.packets
      val num = in.length
      if (num < 10) return b   // don't bother

      // currently, we focus on the three messages
      // most frequently found in sound processes:
      // `/n_set`, `/n_mapan`, `/n_after`.
      // `/n_set` and `/n_mapan` can be collapsed
      // per node-ID; `/n_after` can be all collapsed.
      // To ensure correctness, we must not collapse
      // across `/s_new` and `/g_new` boundaries.
      // For simplicity, we also restrict collapse
      // of `/n_after` to adjacent messages.
      // Another optimization is collapsing adjacent
      // `/n_free` messages

      // XXX TODO - actually, we don't yet optimize `/n_after`

      val out     = new Array[osc.Packet](num)
      var inOff   = 0
      var outOff  = 0

      var setMap    = Map.empty[Int, Int] // node-id to out-offset containing either n_set or s_new
      var mapanMap  = Map.empty[Int, Int] // node-id to out-offset containing n_mapan
      var nAfterIdx = -2
      var nFreeIdx  = -2

      while (inOff < num) {
        val p       = in(inOff)
        val append  = p match {
          case m: message.NodeSet =>
            val id = m.id
            val i = setMap.getOrElse(id, -1)
            val res = i < 0
            if (res) {
              setMap += id -> outOff
            } else {
              out(i) = (out(i): @unchecked) match {  // unfortunately those case classes do not have `copy` methods...
                case n: message.NodeSet =>
                  message.NodeSet(id, compressControlSet(n.pairs, m.pairs): _*)
                case n: message.SynthNew =>
                  message.SynthNew(n.defName, id, n.addAction, n.targetID,
                    compressControlSet(n.controls, m.pairs): _*)
              }
            }
            res

          case m: message.NodeMapan =>
            val id = m.id
            val i = mapanMap.getOrElse(id, -1)
            val res = i < 0
            if (res) {
              mapanMap += id -> outOff
            } else {
              var message.NodeMapan(_, mappings @ _*) = out(i)
              m.mappings.foreach {
                case c @ ControlABusMap.Single(key, _) =>
                  val j = mappings.indexWhere {
                    case ControlABusMap.Single(`key`, _) => true
                    case _ => false
                  }
                  mappings = if (j < 0) mappings :+ c else mappings.updated(j, c)

                case c @ ControlABusMap.Multi(key, _, numChannels) =>
                  val j = mappings.indexWhere {
                    case ControlABusMap.Multi(`key`, _, `numChannels`) => true
                    case _ => false
                  }
                  mappings = if (j < 0) mappings :+ c else mappings.updated(j, c)
              }
              out(i) = message.NodeMapan(id, mappings: _*)
            }
            res

          case m: message.NodeAfter =>
            val res = nAfterIdx != outOff - 1
            if (res) {  // predecessor was not n_after
              nAfterIdx = outOff

              //              // more ambitious:
              //              // collapse a single `/n_after` with an immediate
              //              // preceding `/s_new`.
              //              val g = m.groups
              //              if (g.size == 1) {
              //                val (id, after) = g.head
              //                val newIdx = setMap.getOrElse(id, -1)
              //                if (newIdx >= 0) {
              //                  ...
              //                }
              //              }

            } else {
              val message.NodeAfter(groups @ _*) = out(nAfterIdx)
              out(nAfterIdx) = message.NodeAfter(groups ++ m.groups: _*)
            }
            res

          case m: message.NodeFree =>
            val res = nFreeIdx != outOff - 1
            if (res) {  // predecessor was not n_after
              nFreeIdx = outOff
            } else {
              val message.NodeFree(ids @ _*) = out(nFreeIdx)
              out(nFreeIdx) = message.NodeFree(ids ++ m.ids: _*)
            }
            res

          case m: message.SynthNew =>
            val id = m.id
            // setMap   -= id
            setMap   += id -> outOff
            mapanMap -= id
            true

          case m: message.GroupNew =>
            m.groups.foreach { g =>
              val id = g.groupID
              setMap   -= id
              mapanMap -= id
            }
            true

          case _ => true
        }
        if (append) {
          out(outOff) = p
          outOff += 1
        }
        inOff += 1
      }

      if (outOff == num) b else {
        val outT = new Array[osc.Packet](outOff)
        System.arraycopy(out, 0, outT, 0, outOff)
        val res = osc.Bundle(b.timetag, outT: _*)
        //        Console.err.println("----------- BEFORE COMPRESSION -----------")
        //        osc.Packet.printTextOn(b  , Server.codec, Console.err)
        //        Console.err.println("----------- AFTER  COMPRESSION -----------")
        //        osc.Packet.printTextOn(res, Server.codec, Console.err)
        res
      }
    }

    // ---- side effects ----

    private def splitAndSend[A, B](init: A, iter: Iterator[osc.Packet], addSize: Int)(fun: Vec[osc.Packet] => B)
                                  (combine: (A, B) => A): A = {
      @tailrec def loop(a: A, sz: Int, builder: mutable.Builder[osc.Packet, Vec[osc.Packet]]): A =
        if (iter.isEmpty) {
          val res = builder.result()
          if (res.nonEmpty) {
            val a1 = fun(res)
            val a2  = combine(a, a1)
            a2
          } else a

        } else {
          val next  = iter.next()
          val sz1   = next.encodedSize(Server.codec) + 4
          val sz2   = sz + sz1
          if (sz2 > MaxOnlinePacketSize) {
            val res = builder.result()
            if (res.isEmpty) sys.error(s"Cannot encode packet -- too large ($sz1)")
            val a1  = fun(res)
            val a2  = combine(a, a1)
            val newBuilder = Vec.newBuilder[osc.Packet]
            newBuilder += next
            loop(a2, 16 + addSize + sz1, newBuilder)
          } else {
            builder += next
            loop(a, sz2, builder)
          }
        }

      loop(init, 16 + addSize, Vec.newBuilder[osc.Packet])
    }

    def ! (p0: osc.Packet): Unit = {
      val p = p0 match {
        case b0: osc.Bundle if USE_COMPRESSION => compress(b0)
        case _ => p0
      }

      p match {
        case b: osc.Bundle if VERIFY_BUNDLE_SIZE =>
          val sz0 = Server.codec.encodedBundleSize(b)
          if (sz0 <= MaxOnlinePacketSize) {
            peer ! b
          } else {
            // Since the bundle is synchronous, it is not trivial to split it
            // into several bundles. And if we split at arbitrary points some
            // very bad things could happen, for example sound or feedback
            // going to wrong temporary buses, so this is absolutely forbidden.
            //
            // We take this as an "emergency" branch that one should avoid to
            // be taken at all costs. The solution for this branch is that we
            // temporarily pause the server's default group. That way no
            // damage can be done, but it may result in a short noticable bit
            // of silence. That's as good as it gets, I suppose...
            Console.err.println(s"WARNING: Bundle size $sz0 exceeds $MaxOnlinePacketSize. Splitting into multiple bundles")
            val gid   = peer.defaultGroup.id
            val iter  =
              Iterator.single(message.NodeRun(gid -> false)) ++ b.packets.iterator ++
              Iterator.single(message.NodeRun(gid -> true ))

            splitAndSend[Unit, Unit](init = (), iter = iter, addSize = 0) { packets =>
              peer ! osc.Bundle(b.timetag, packets: _*)
            } ((_, _) => ())
          }

        case _ =>
          peer ! p
      }
    }

    def !! (b0: osc.Bundle): Future[Unit] = {
      val b   = if (USE_COMPRESSION) compress(b0) else b0
      val tt  = b.timetag
      if (VERIFY_BUNDLE_SIZE) {
        val sz0     = Server.codec.encodedBundleSize(b)
        if (sz0 + 20 <= MaxOnlinePacketSize) {
          perform_!!(tt, b.packets)
        } else {
          val iter = b.packets.iterator
          val futures = splitAndSend[Vec[Future[Unit]], Future[Unit]](init = Vector.empty,
                                                                      iter = iter, addSize = 20) { packets =>
            perform_!!(tt, packets)
          } (_ :+ _)
          Future.reduce[Unit, Unit](futures)((_, _) => ()) // reduceLeft requires Scala 2.12
        }
      } else {
        perform_!!(tt, b.packets)
      }
    }

    private def perform_!!(tt: Timetag, packets: Seq[osc.Packet]): Future[Unit] = {
      val syncMsg = peer.syncMsg()
      val syncID  = syncMsg.id
      val bndlS   = osc.Bundle(tt, packets :+ syncMsg: _*)
      peer.!!(bndlS) {
        case message.Synced(`syncID`) =>
      }
    }

    def commit(future: Future[Unit]): Unit = ()  // we don't use these
  }

  private final case class OfflineImpl(peer: SServer) extends Impl with Server.Offline {
    override def toString = s"$peer @offline"

    def isLocal      : Boolean  = true
    def isRealtime   : Boolean  = false
    def maxPacketSize: Int      = MaxOfflinePacketSize

    private val sync = new AnyRef

    var position  = 0L

    private var _bundles      = Vector.empty[osc.Bundle]
    private var _commits      = Vector.empty[Future[Unit]]

    private def time: Double  = position / sampleRate

    def committed(): Future[Unit] = sync.synchronized {
      val futures       = filteredCommits
      _commits          = Vector.empty
      // implicit val exec = peer.clientConfig.executionContext
      ServerImpl.reduceFutures(futures)
    }

    def bundles(addDefaultGroup: Boolean): Vec[osc.Bundle] = sync.synchronized {
      val res   = _bundles
      _bundles  = Vector.empty
      val res1  = if (res.isEmpty || !addDefaultGroup) res else {
        val b   = osc.Bundle(res.head.timetag,
          message.GroupNew(message.GroupNew.Data(groupID = 1, addAction = addToHead.id, targetID = 0)))
        b +: res
      }
      res1
    }

    private def addBundle(b: osc.Bundle): Unit = sync.synchronized {
      val b1 = if (b.timetag == osc.Timetag.now) osc.Bundle.secs(time, b: _*) else b
      val sz = Server.codec.encodedBundleSize(b1)
      // SuperCollider versions until 2014 have a hard-coded limit of 8K bundles in NRT!
      // cf. https://github.com/supercollider/supercollider/commit/f3f0f81de4259aa44983f1041589f895c91798a1
      val szOk = sz <= MaxOfflinePacketSize
      if (szOk || b1.length == 1) {
        log(s"addBundle $b1")
        if (!szOk) log(s"addBundle - bundle exceeds ${MaxOfflinePacketSize/1024}k!")
        _bundles :+= b1
      } else {
        val tt = b1.timetag
        b.foreach(p => addBundle(osc.Bundle(tt, p)))
      }
    }

    def ! (p: osc.Packet): Unit = {
      val b = p match {
        case m : osc.Message  => osc.Bundle.secs(time, m)
        case b0: osc.Bundle   => b0
      }
      addBundle(b)
    }

    def !! (bndl: osc.Bundle): Future[Unit] = {
      addBundle(bndl)
      Future.successful(())
    }

    // caller must have `sync`
    private def filteredCommits = _commits.filterNot(_.isCompleted)

    def commit(future: Future[Unit]): Unit =
      sync.synchronized {
        _commits = filteredCommits :+ future
      }
  }

  private abstract class Impl extends Server { server =>

    implicit def executionContext: ExecutionContext = peer.clientConfig.executionContext

    final private[this] val controlBusAllocator = BlockAllocator("control", peer.config.controlBusChannels)
    final private[this] val audioBusAllocator   = BlockAllocator("audio"  , peer.config.audioBusChannels, peer.config.internalBusIndex)
    final private[this] val bufferAllocator     = BlockAllocator("buffer" , peer.config.audioBuffers)
    final private[this] val nodeAllocator       = NodeIDAllocator(peer.clientConfig.clientID, peer.clientConfig.nodeIDOffset)

    final val defaultGroup: Group = Group.wrap(this, peer.defaultGroup)

    final def config      : Server .Config = peer.config
    final def clientConfig: SClient.Config = peer.clientConfig

    final def sampleRate: Double              = peer.sampleRate
    final def counts    : message.StatusReply = peer.counts

    final def allocControlBus(numChannels: Int)(implicit tx: Txn): Int = {
      val res = controlBusAllocator.alloc(numChannels)
      if (res < 0) throw AllocatorExhausted(s"Control buses exhausted for $this")
      res
    }

    final def allocAudioBus(numChannels: Int)(implicit tx: Txn): Int = {
      val res = audioBusAllocator.alloc(numChannels)
      if (res < 0) throw AllocatorExhausted(s"Audio buses exhausted for $this")
      res
    }

    final def freeControlBus(index: Int, numChannels: Int)(implicit tx: Txn): Unit =
      controlBusAllocator.free(index, numChannels)

    final def freeAudioBus(index: Int, numChannels: Int)(implicit tx: Txn): Unit =
      audioBusAllocator.free(index, numChannels)

    final def allocBuffer(numConsecutive: Int)(implicit tx: Txn): Int = {
      val res = bufferAllocator.alloc(numConsecutive)
      if (res < 0) throw AllocatorExhausted(s"Buffers exhausted for $this")
      res
    }

    final def freeBuffer(index: Int, numConsecutive: Int)(implicit tx: Txn): Unit =
      bufferAllocator.free(index, numConsecutive)

    final def nextNodeID()(implicit tx: Txn): Int = nodeAllocator.alloc()

    // ---- former Server ----

    private type T = Topology[NodeRef, NodeRef.Edge]

    final private[this] val ugenGraphMap  = TMap.empty[IndexedSeq[Byte], SynthDef]
    final private[this] val synthDefLRU   = Ref(Vector.empty[(IndexedSeq[Byte], SynthDef)])

    // limit on number of online defs XXX TODO -- head room rather arbitrary
    final private[this] val maxDefs       = math.max(128, server.config.maxSynthDefs - 128)

    final private[this] val topologyRef = Ref[T](Topology.empty[NodeRef, NodeRef.Edge])

    final def topology(implicit tx: Txn): T = topologyRef()

    final def acquireSynthDef(graph: UGenGraph, nameHint: Option[String])(implicit tx: Txn): SynthDef = {
      val bos   = new ByteArrayOutputStream
      val dos   = new DataOutputStream(bos)
      graph.write(dos, version = 1) // Escape.write(graph, dos)
      dos.flush()
      dos.close()
      val bytes = bos.toByteArray
      val equ: IndexedSeq[Byte] = bytes // opposed to plain `Array[Byte]`, this has correct definition of `equals`
      log(s"request for synth graph ${equ.hashCode()}")

      ugenGraphMap.get(equ).fold[SynthDef] {
        log(s"synth graph ${equ.hashCode()} is new")
        if (VERIFY_WIRE_BUFFERS) {
          val wires     = UGenGraph.calcWireBuffers(graph)
          val maxWires  = server.peer.config.wireBuffers
          if (wires > maxWires) {
            val nameS = nameHint.fold("")(n => s" for '$n'")
            throw new IndexOutOfBoundsException(s"UGen graph$nameS exceeds number of wire buffers ($wires > $maxWires")
          }
        }

        val name  = mkSynthDefName(nameHint)
        val peer  = de.sciss.synth.SynthDef(name, graph)
        val rd    = impl.SynthDefImpl(server, peer)
        val lru   = synthDefLRU.transformAndGet((equ, rd) +: _)
        if (lru.size == maxDefs) {
          val init :+ Tuple2(lastEqu, lastDf) = lru
          log(s"purging synth-def ${lastDf.name}")
          lastDf.dispose()
          ugenGraphMap.remove(lastEqu)
          synthDefLRU() = init
        }
        rd.recv()
        ugenGraphMap.put(equ, rd)
        rd
      } { rd =>
        synthDefLRU.transform { xs =>
          val idx = xs.indexWhere(_._1 == equ)
          val ys = xs.patch(idx, Nil, 1)  // remove from old spot
          (equ, rd) +: ys // put to head as most recently used item
        }
        rd
      }
    }

    final def addVertex(node: NodeRef)(implicit tx: Txn): Unit = {
      log(s"Server.addVertex($node)")
      topologyRef.transform(_.addVertex(node))
    }

    final def removeVertex(node: NodeRef)(implicit tx: Txn): Unit = {
      log(s"Server.removeVertex($node)")
      topologyRef.transform(_.removeVertex(node))
    }

    final def addEdge(edge: NodeRef.Edge)(implicit tx: Txn): Boolean = {
      log(s"Server.addEdge($edge)")
      val res = topologyRef().addEdge(edge)
      res.foreach { case (topNew, moveOpt) =>
        topologyRef() = topNew
        moveOpt.foreach {
          case Topology.MoveAfter (ref, aff) =>
            val refNode = ref.node
            aff.reverseIterator.foreach { x =>
              val xNode = x.node
              xNode.moveAfter(refNode)
            }
          case Topology.MoveBefore(ref, aff) =>
            val refNode = ref.node
            aff.foreach { x =>
              val xNode = x.node
              xNode.moveBefore(refNode)
            }
        }
      }
      res.isSuccess
    }

    final def removeEdge(edge: NodeRef.Edge)(implicit tx: Txn): Unit = {
      log(s"Server.removeEdge($edge)")
      topologyRef.transform(_.removeEdge(edge))
    }

    final private[this] val uniqueDefID = Ref(0)

    final private[this] def allCharsOk(name: String): Boolean = {
      val len = name.length
      var i   = 0
      while (i < len) {
        val c   = name.charAt(i).toInt
        val ok  = c > 36 && c < 123 || c != 95 // in particular, disallow underscore
        if (!ok) return false
        i += 1
      }
      true
    }

    final def mkSynthDefName(nameHint: Option[String])(implicit tx: Txn): String = {
      val id = nextDefID()
      abbreviate(nameHint.getOrElse("proc"), s"_$id")
    }

    final private[this] def abbreviate(name: String, suffix: String): String = {
      val len = name.length
      val lim = 16 - suffix.length
      val sb  = new StringBuffer(16)
      if ((len <= lim) && allCharsOk(name)) {
        sb.append(name)
      } else {
        var i   = 0
        while (i < len && sb.length() < lim) {
          val c = name.charAt(i).toInt
          val ok = c > 36 && c < 123 || c != 95 // in particular, disallow underscore
          if (ok) sb.append(c.toChar)
          i += 1
        }
      }
      sb.append(suffix)
      sb.toString
    }

    final private[this] def nextDefID()(implicit tx: Txn): Int =
      uniqueDefID.getAndTransform(_ + 1)

    // ---- sending ----

    final private[this] val msgStampRef = Ref(0)

    final private[synth] def messageTimeStamp: Ref[Int] = msgStampRef

    final private[this] val sync            = new AnyRef
    final private[this] var bundleWaiting   = Map.empty[Int, Vec[Scheduled]]
    final private[this] var bundleReplySeen = -1

    final private[this] class Scheduled(val bundle: Txn.Bundle, promise: Promise[Unit]) {
      def apply(): Future[Unit] = {
        val fut = sendNow(bundle)
        promise.completeWith(fut)
        fut
      }
    }

    final private[this] def sendAdvance(stamp: Int): Future[Unit] = {
      if (DEBUG) println(s"ADVANCE $stamp")
      val futures: Vec[Future[Unit]] = sync.synchronized {
        val i = bundleReplySeen + 1
        if (i <= stamp) {
          bundleReplySeen = stamp
          val funs = (i to stamp).flatMap { j =>
            bundleWaiting.get(j) match {
              case Some(_funs)  => bundleWaiting -= j; _funs
              case _            => Vector.empty
            }
          }
          funs.map(_.apply())
        }
        else Vector.empty
      }
      reduceFutures(futures)
    }

    final private[this] def sendNow(bundle: Txn.Bundle): Future[Unit] = {
      import bundle.{msgs, stamp}
      if (msgs.isEmpty) return sendAdvance(stamp) // Future.successful(())

      val allSync = (stamp & 1) == 1
      if (DEBUG) println(s"SEND NOW $msgs - allSync? $allSync; stamp = $stamp")
      if (allSync) {
        val p = msgs match {
          case Vec(msg) /* if allSync */ => msg
          case _ => osc.Bundle.now(msgs: _*)
        }
        server ! p
        sendAdvance(stamp)

      } else {
        val bndl  = osc.Bundle.now(msgs: _*)
        val fut   = server.!!(bndl)
        val futR  = fut.recover {
          case message.Timeout() =>
            log("TIMEOUT while sending OSC bundle!")
        }
        futR.flatMap(_ => sendAdvance(stamp))
      }
    }

    final def send(bundles: Txn.Bundles): Future[Unit] = {
      val res = sync.synchronized {
        val (now, later) = bundles.partition(bundleReplySeen >= _.depStamp)
        // it is important to process the 'later' bundles first,
        // because now they might rely on some `bundleReplySeen` that is
        // increased by processing the `now` bundles.
        val futuresLater  = later.map { m =>
          val p   = Promise[Unit]()
          val sch = new Scheduled(m, p)
          bundleWaiting += m.depStamp -> (bundleWaiting.getOrElse(m.depStamp, Vector.empty) :+ sch)
          p.future
        }
        val futuresNow    = now.map(sendNow)
        reduceFutures(futuresNow ++ futuresLater)
      }

      server.commit(res)
      res
    }
  }
}