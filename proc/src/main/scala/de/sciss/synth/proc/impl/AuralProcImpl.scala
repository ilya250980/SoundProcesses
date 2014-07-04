package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.synth._
import de.sciss.numbers
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.Curve.parametric
import de.sciss.synth.proc.Scan.Link
import de.sciss.synth.{addToHead, ControlSet}
import UGenGraphBuilder.MissingIn
import de.sciss.synth.proc.{logAural => logA}
import AuralObj.ProcData

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm._

object AuralProcImpl extends AuralObj.Factory {
  type E[S <: evt.Sys[S]] = Proc.Elem[S]

  def typeID = ElemImpl.Proc.typeID

  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val data = context.acquire[AuralObj.ProcData[S]](proc) {
      val ugenInit = UGenGraphBuilder.init(proc)
      val data0 = new DataImpl(tx.newHandle(proc), ugenInit)
      data0.tryBuild()
      data0
    }
    val res = new Impl(data)
    res
  }

  private final class OutputBuilder(val bus: AudioBus) {
    var sinks = List.empty[(String, AuralNode)]
  }

  private final class AuralProcBuilder[S <: Sys[S]](val ugen: UGenGraphBuilder[S] /*, val name: String */) {
    var outputs = Map.empty[String, OutputBuilder]
  }

  private type ObjSource[S <: Sys[S]] = stm.Source[S#Tx, Obj.T[S, Proc.Elem]]

  private final class DataImpl[S <: Sys[S]](val obj: ObjSource[S], state0: UGenGraphBuilder.State[S])
                                              (implicit context: AuralContext[S])
    extends ProcData[S] {

    private val procLoc  = TxnLocal[Obj.T[S, Proc.Elem]]()
    private val stateRef = Ref[UGenGraphBuilder.State[S]](state0)

    // def server: Server = context.server

    def dispose()(implicit tx: S#Tx): Unit = {
      // nothing yet
    }

    def state(implicit tx: S#Tx) = stateRef.get(tx.peer)

    def tryBuild()(implicit tx: S#Tx): Unit = {
      state match {
        case s0: UGenGraphBuilder.Incomplete[S] =>
          val s1 = s0.retry(this)
          stateRef.set(s1)(tx.peer)

        case s0: UGenGraphBuilder.Complete[S] => // nada
      }
    }

    def procCached()(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = {
      implicit val itx = tx.peer
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    // called by UGenGraphBuilderImpl
    def attrNumChannels(key: String)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      procObj.attr.getElem(key).fold(1) {
        case a: DoubleVecElem[S]     => a.peer.value.size // XXX TODO: would be better to write a.peer.size.value
        case a: AudioGraphemeElem[S] => a.peer.spec.numChannels
        case _ => 1
      }
    }
    // called by UGenGraphBuilderImpl
    def scanInNumChannels(key: String, numChannels: Int)(implicit tx: S#Tx): Int = {
      val procObj = procCached()
      val proc    = procObj.elem.peer
      val numCh   = proc.scans.get(key).fold(0) { scan =>
        val chans = scan.sources.toList.map {
          case Link.Grapheme(peer) =>
            // val chansOpt = peer.valueAt(time).map(_.numChannels)
            // chansOpt.getOrElse(numChannels)
            peer.numChannels

          case Link.Scan(peer) =>
            // val sourceOpt = scanMap.get(peer.id)
            val sourceOpt = context.getAux[(String, ProcData[S])](peer.id)
            val chansOpt = sourceOpt.flatMap {
              case (sourceKey, sourceData) =>
                // val sourceObj = sourceObjH()
                // getOutputBus(sourceObj, sourceKey)
                sourceData.state.scanOuts.get(sourceKey)
            }
            chansOpt.getOrElse(throw MissingIn(peer))
        }
        if (chans.isEmpty) 0 else chans.max
      }
      math.max(1, numCh)
    }

    //    private def getOutputBus(obj0: Obj.T[S, Proc.Elem], key: String)(implicit tx: S#Tx): Option[AudioBus] =
    //      context.get(obj0) match {
    //        case Some(data0) =>
    //          ... // data0.getOutputBus(key)
    //        case _ =>
    //          ...
    //          //          assert(ongoingBuild.isInitialized(tx.peer))
    //          //          val ob = ongoingBuild.get(tx.peer)
    //          //          for {
    //          //            map <- ob.idMap
    //          //            pb  <- map.get(timedID)
    //          //            out <- pb.outputs.get(key)
    //          //          } yield out.bus
    //      }
  }

  // ---------------------------------------------------------------------

  private final class Impl[S <: Sys[S]](private val data: AuralObj.ProcData[S])(implicit context: AuralContext[S])
    extends AuralObj.Proc[S] with ObservableImpl[S, AuralObj.State] {

    import context.server

    def obj: stm.Source[S#Tx, Obj.T[S, Proc.Elem]] = data.obj

    def typeID: Int = Proc.typeID

    // def latencyEstimate(implicit tx: S#Tx): Long = ...

    //    private def addFlush()(implicit tx: S#Tx): Unit = {
    //      logA(s"addFlush (${hashCode.toHexString})")
    //      tx.beforeCommit(flush()(_))
    //      // concurrent.stm.Txn.afterRollback(status => logA(s"rollback $status !!"))(tx.peer)
    //    }
    //
    //    // called before the transaction successfully completes.
    //    // this is the place where we launch completely built procs.
    //    private def flush()(ptx: Txn): Unit = {
    //      val itx = ptx.peer
    //      ongoingBuild.get(itx).seq.foreach { builder =>
    //        val ugen = builder.ugen
    //        if (ugen.isComplete) {
    //          try {
    //            launchProc(builder)
    //          } catch {
    //            case NonFatal(e) =>
    //              e.printStackTrace()
    //              throw e
    //          }
    //
    //        } else {
    //          // XXX TODO: do we need to free buses associated with ugen.scanOuts ?
    //          println("Warning: Incomplete aural proc build for " + ugen.timed.value)
    //        }
    //      }
    //    }

    //    private def procAdded(time: Long, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
    //      logA(s"added $timed (${hashCode.toHexString})")
    //
    //      val timedID = timed.id
    //      val ugen    = UGenGraphBuilder(this)
    //      val builder = new AuralProcBuilder(ugen /*, name */)
    //      //      val newTxn  = !ongoingBuild.isInitialized(tx.peer)
    //      //      if (newTxn) addFlush() // ( ProcTxn() )   // the next line (`ongoingBuild.get`) will initialise then
    //      //      val ongoing = ongoingBuild.get(tx.peer)
    //      //      ongoing.seq :+= builder
    //      //      // assert(ongoingBuild.isInitialized(tx.peer))
    //
    //      //      // initialise the id-to-builder map if necessary
    //      //      val builderMap = ongoing.idMap.getOrElse {
    //      //        val m = tx.newInMemoryIDMap[AuralProcBuilder[S]]
    //      //        ongoing.idMap = Some(m)
    //      //        m
    //      //      }
    //      //      // add the builder to it.
    //      //      builderMap.put(timedID, builder)
    //
    //      // store the look up information for the scans
    //      // (this is only needed because Scan.Link.Scan reveals
    //      // only the Scan which in turn doesn't currently carry
    //      // key and proc information, so it can't be recovered
    //      // otherwise; in the future this may change)
    //      val proc  = timed.value.elem.peer
    //      val scans = proc.scans
    //      scans.iterator.foreach {
    //        case (key, scan) =>
    //          import de.sciss.lucre.synth.expr.IdentifierSerializer
    //          scanMap.put(scan.id, key -> tx.newHandle(timedID))
    //      }
    //
    //      incrementalBuild(ongoing, builder)
    //    }

    private val currentStateRef = Ref[AuralObj.State](AuralObj.Stopped)
    private val targetStateRef  = Ref[AuralObj.State](AuralObj.Stopped)

    def state(implicit tx: S#Tx): AuralObj.State = currentStateRef.get(tx.peer)

    def play(time: SpanLike)(implicit tx: S#Tx): Unit = {
      val oldTarget = targetStateRef.swap(AuralObj.Playing)(tx.peer)
      val curr      = state
      data.state match {
        case s: UGenGraphBuilder.Complete[S] => launchProc(s, time)
        case _ =>
      }
    }

    def stop(time: Long)(implicit tx: S#Tx): Unit = {

    }

    def prepare()(implicit tx: S#Tx): Unit = ???

    private def launchProc(ugen: UGenGraphBuilder.Complete[S], span: SpanLike)(implicit tx: S#Tx): Unit = {
      // val ugen          = builder.ugen
      // val timed         = ugen.timed

      val p             = data.procCached()

      // logA(s"begin launch $timed (${hashCode.toHexString})")
      logA(s"begin launch $p (${hashCode.toHexString})")

      val ug            = ugen.result
      implicit val itx  = tx.peer

      // val time          = ugen.time
      // val p             = timed.value

      val nameHint      = p.attr.expr[String](ProcKeys.attrName).map(_.value)
      val synth         = Synth.expanded(server, ug, nameHint = nameHint)
      // users are elements which must be added after the aural proc synth is started, and removed when it stops
      var users         = List.empty[DynamicUser]
      // resources are dependencies in terms of synth bundle spawning, and will be disposed by the aural proc
      var dependencies  = List.empty[Resource]

      // ---- handle input buses ----
      // val span          = timed.span.value
      var setMap        = Vec[ControlSet](
//        graph.Time    .key -> time / sampleRate,
//        graph.Offset  .key -> (span match {
//          case Span.HasStart(start) => (time - start) / sampleRate
//          case _ => 0.0
//        }),
//        graph.Duration.key -> (span match {
//          case Span(start, stop)  => (stop - start) / sampleRate
//          case _ => Double.PositiveInfinity
//        })
      )

      import Timeline.{SampleRate => sampleRate}

      // ---- attributes ----
      val attrNames     = ugen.attributeIns
      if (attrNames.nonEmpty) attrNames.foreach { n =>
        val ctlName = graph.attribute.controlName(n)
        p.attr.getElem(n).foreach {
          case a: IntElem     [S] => setMap :+= (ctlName -> a.peer.value.toFloat: ControlSet)
          case a: DoubleElem  [S] => setMap :+= (ctlName -> a.peer.value.toFloat: ControlSet)
          case a: BooleanElem [S] => setMap :+= (ctlName -> (if (a.peer.value) 1f else 0f): ControlSet)
          case a: FadeSpec.Elem[S] =>
            val spec = a.peer.value
            // dur, shape-id, shape-curvature, floor
            val values = Vec(
              (spec.numFrames / sampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
                case parametric(c)  => c
                case _              => 0f
              }, spec.floor
            )
            setMap :+= (ctlName -> values: ControlSet)
          case a: DoubleVecElem[S] =>
            val values = a.peer.value.map(_.toFloat)
            setMap :+= (ctlName -> values: ControlSet)
          case a: AudioGraphemeElem[S] =>
            val audioElem = a.peer
            val spec      = audioElem.spec
            //              require(spec.numChannels == 1 || spec.numFrames == 1,
            //                s"Audio grapheme ${a.peer} must have either 1 channel or 1 frame to be used as scalar attribute")
            require(spec.numFrames == 1, s"Audio grapheme ${a.peer} must have exactly 1 frame to be used as scalar attribute")
            //              val numChL = if (spec.numChannels == 1) spec.numFrames else spec.numChannels
            //              require(numChL <= 4096, s"Audio grapheme size ($numChL) must be <= 4096 to be used as scalar attribute")
            val numCh  = spec.numChannels // numChL.toInt
            require(numCh <= 4096, s"Audio grapheme size ($numCh) must be <= 4096 to be used as scalar attribute")
            val b      = Bus.control(server, numCh)
            val res    = BusNodeSetter.mapper(ctlName, b, synth)
            users ::= res
            val w      = AudioArtifactScalarWriter(b, audioElem.value)
            dependencies     ::= w
          // users ::= w

          case a => sys.error(s"Cannot cast attribute $a to a scalar value")
        }
      }

      // ---- streams ----
      val streamNames = ugen.streamIns
      if (streamNames.nonEmpty) streamNames.foreach { case (n, infoSeq0) =>
        val infoSeq = if (infoSeq0.isEmpty) UGenGraphBuilder.StreamIn.empty :: Nil else infoSeq0

        infoSeq.zipWithIndex.foreach { case (info, idx) =>
          val ctlName     = graph.stream.controlName(n, idx)
          val bufSize     = if (info.isEmpty) server.config.blockSize else {
            val maxSpeed  = if (info.maxSpeed <= 0.0) 1.0 else info.maxSpeed
            val bufDur    = 1.5 * maxSpeed
            val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
            val bestSz    = math.max(minSz, (bufDur * sampleRate).toInt)
            import numbers.Implicits._
            val bestSzHi  = bestSz.nextPowerOfTwo
            val bestSzLo  = bestSzHi >> 1
            if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
          }
          val (rb, gain) = p.attr.getElem(n).fold[(Buffer, Float)] {
            // DiskIn and VDiskIn are fine with an empty non-streaming buffer, as far as I can tell...
            // So instead of aborting when the attribute is not set, fall back to zero
            val _buf = Buffer(server)(numFrames = bufSize, numChannels = 1)
            (_buf, 0f)
          } {
            case a: AudioGraphemeElem[S] =>
              val audioElem = a.peer
              val spec      = audioElem.spec
              val path      = audioElem.artifact.value.getAbsolutePath
              val offset    = audioElem.offset  .value
              val _gain     = audioElem.gain    .value
              val _buf      = if (info.isNative) {
                Buffer.diskIn(server)(
                  path          = path,
                  startFrame    = offset,
                  numFrames     = bufSize,
                  numChannels   = spec.numChannels
                )
              } else {
                val __buf = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
                val trig = new StreamBuffer(key = n, idx = idx, synth = synth, buf = __buf, path = path,
                  fileFrames = spec.numFrames, interp = info.interp)
                trig.install()
                __buf
              }
              (_buf, _gain.toFloat)

            case a => sys.error(s"Cannot use attribute $a as an audio stream")
          }
          setMap      :+= (ctlName -> Seq[Float](rb.id, gain): ControlSet)
          dependencies        ::= rb
        }
      }

      import Grapheme.Segment
      // val outBuses      = builder.outputs
      // val aural         = AuralNode(synth, outBuses.mapValues(_.bus))
      val aural         = AuralNode(synth, Map.empty)

      // ---- scans ----
      ugen.scanIns.foreach {
        case (key, scanIn) =>
          val numCh = scanIn.numChannels

          @inline def ensureChannels(n: Int): Unit =
            require(n == numCh, s"Scan input changed number of channels (expected $numCh but found $n)")

          val inCtlName = graph.scan.inControlName(key)
          // var inBus     = Option.empty[AudioBusNodeSetter]

          lazy val lazyInBus: AudioBusNodeSetter = {
            val b      = Bus.audio(server, numCh)
            val res    = if (scanIn.fixed)
              BusNodeSetter.reader(inCtlName, b, synth)
            else
              BusNodeSetter.mapper(inCtlName, b, synth)
            users ::= res
            aural.addInputBus(key, res.bus)
            res
          }

          // note: if not found, stick with default

          // XXX TODO:
          val time = span match {
            case hs: Span.HasStart => hs.start
            case _ => 0L
          }

          // XXX TODO: combination fixed + grapheme source doesn't work -- as soon as there's a bus mapper
          //           we cannot use ControlSet any more, but need other mechanism
          p.elem.peer.scans.get(key).foreach { scan =>
            val src = scan.sources
            // if (src.isEmpty) {
            if (scanIn.fixed) lazyInBus  // make sure a fixed channels scan in exists as a bus
            // } else {
            src.foreach {
              case Link.Grapheme(peer) =>
                val segmOpt = peer.segment(time)
                segmOpt.foreach {
                  // again if not found... stick with default
                  case const: Segment.Const =>
                    ensureChannels(const.numChannels) // ... or could just adjust to the fact that they changed
                    //                        setMap :+= ((key -> const.numChannels) : ControlSet)
                    setMap :+= (if (const.numChannels == 1) {
                      ControlSet.Value (inCtlName, const.values.head .toFloat )
                    } else {
                      ControlSet.Vector(inCtlName, const.values.map(_.toFloat))
                    })

                  case segm: Segment.Curve =>
                    ensureChannels(segm.numChannels) // ... or could just adjust to the fact that they changed
                  // println(s"segment : ${segm.span}")
                  val bm     = lazyInBus
                    val w      = SegmentWriter(bm.bus, segm, time, sampleRate)
                    dependencies     ::= w
                  // users ::= w

                  case audio: Segment.Audio =>
                    ensureChannels(audio.numChannels)
                    val bm     = lazyInBus
                    val w      = AudioArtifactWriter(bm.bus, audio, time, sampleRate)
                    dependencies     ::= w
                  // users    ::= w
                }

              case Link.Scan(peer) => ???
//                scanMap.get(peer.id).foreach {
//                  case (srcKey, idH) =>
//                    val srcTimedID  = idH()
//                    val bIn         = lazyInBus
//
//                    // if the source isn't found (because it's probably in the ongoing build),
//                    // we ignore that here; there is a symmetric counter part, looking for the
//                    // builder.outputs that will handle these cases.
//                    viewMap.get(srcTimedID).foreach { srcAural =>
//                      val bOut    = srcAural.getOutputBus(srcKey).getOrElse {
//                        sys.error(s"Source bus disappeared $srcTimedID -> $srcKey")
//                      }
//                      ensureChannels(bOut.numChannels)
//                      val edge    = NodeGraph.Edge(srcAural, srcKey, aural, key)
//                      val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn.bus)
//                      dependencies      ::= link
//                      users     ::= link
//                    }
//                }
            }
            // }
          }
      }

      // ---- handle output buses, and establish missing links to sinks ----
      if (ugen.scanOuts.nonEmpty) ???
//      builder.outputs.foreach {
//        case (key, out) =>
//          val bw     = BusNodeSetter.writer(scan.outControlName(key), out.bus, synth)
//          val bOut   = bw.bus
//          users :+= bw
//
//          p.elem.peer.scans.get(key).foreach { scan =>
//            scan.sinks.foreach {
//              case Link.Scan(peer) =>
//                scanMap.get(peer.id).foreach {
//                  case (sinkKey, idH) =>
//                    val sinkTimedID = idH()
//                    viewMap.get(sinkTimedID).foreach { sinkAural =>
//                      val bIn = sinkAural.getInputBus(sinkKey).getOrElse {
//                        sys.error(s"Sink bus disappeared $sinkTimedID -> $sinkKey")
//                      }
//                      require(bIn.numChannels == bOut.numChannels,
//                        s"Scan input changed number of channels (expected ${bOut.numChannels} but found ${bIn.numChannels})")
//                      val edge    = NodeGraph.Edge(aural, key, sinkAural, sinkKey)
//                      val link    = AudioLink(edge, sourceBus = bOut, sinkBus = bIn)
//                      dependencies      ::= link
//                      users     ::= link
//                    }
//                }
//
//              case _  =>
//            }
//          }
//      }

      // busUsers.foreach(_.add())

      // XXX TODO
      val group = server.defaultGroup

      aural.init(users, dependencies)
      // wrap as AuralProc and save it in the identifier map for later lookup
      synth.play(target = group, addAction = addToHead, args = setMap, dependencies = dependencies)
      if (users.nonEmpty) users.foreach(_.add())

      // if (setMap.nonEmpty) synth.set(audible = true, setMap: _*)
      logA(s"launched $p -> $aural (${hashCode.toHexString})")
      // viewMap.put(timed.id, aural)
    }
  }
}