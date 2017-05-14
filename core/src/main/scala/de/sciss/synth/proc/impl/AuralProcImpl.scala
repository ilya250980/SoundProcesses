/*
 *  AuralProcImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.file._
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{DoubleVector, Expr, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, TxnLike}
import de.sciss.lucre.synth.{AudioBus, Buffer, Bus, BusNodeSetter, NodeRef, Server, Sys}
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.ControlSet
import de.sciss.synth.io.AudioFileType
import de.sciss.synth.proc.AuralObj.{TargetPlaying, TargetPrepared, TargetState, TargetStop}
import de.sciss.synth.proc.AuralView.{Playing, Prepared, Preparing, Stopped}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.TimeRef.SampleRate
import de.sciss.synth.proc.UGenGraphBuilder.{Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.graph.impl.{ActionResponder, StopSelfResponder}
import de.sciss.synth.proc.{UGenGraphBuilder => UGB, logAural => logA}

import scala.concurrent.Future
import scala.concurrent.stm.{Ref, TMap, TxnLocal}

object AuralProcImpl {
  def apply[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val res = new Impl[S]
    res.init(proc)
  }

  private final class ObservedGenView[S <: Sys[S]](val gen: GenView[S], obs: Disposable[S#Tx])
    extends Disposable[S#Tx] {

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      gen.dispose()
    }
  }

  final class BufferAndGain(val buf: Buffer, val gain: Float)

  class Impl[S <: Sys[S]](implicit val context: AuralContext[S])
    extends AuralObj.Proc[S]
    with UGB.Context[S]
    with AuralAttribute.Observer[S]
    with ObservableImpl[S, AuralView.State] { impl =>

    import TxnLike.peer
    import context.{scheduler => sched}
    import sched.cursor

    private[this] val buildStateRef = Ref.make[UGB.State[S]]()

    // running attribute inputs
    private[this] val auralAttrMap  = TMap.empty[String, AuralAttribute[S]]
    private[this] val outputBuses   = TMap.empty[String, AudioBus]
    private[this] val auralOutputs  = TMap.empty[String, AuralOutput.Owned[S]]

    private[this] val genViewMap    = TMap.empty[String, ObservedGenView[S]]

    private[this] val procLoc       = TxnLocal[Proc[S]]() // cache-only purpose

    private[this] var observers     = List.empty[Disposable[S#Tx]]

    private[this] var _obj: stm.Source[S#Tx, Proc[S]] = _

    final def obj: stm.Source[S#Tx, Proc[S]] = _obj

    override def toString = s"AuralObj.Proc@${hashCode().toHexString}"

    final def server: Server = context.server

    object ports extends ObservableImpl[S, AuralObj.Proc.Update[S]] {
      def apply(update: AuralObj.Proc.Update[S])(implicit tx: S#Tx): Unit = fire(update)
    }

    def getAttr  (key: String)(implicit tx: S#Tx): Option[AuralAttribute[S]] = auralAttrMap.get(key)
    def getOutput(key: String)(implicit tx: S#Tx): Option[AuralOutput   [S]] = auralOutputs.get(key)

    /* The ongoing build aural node build process, as stored in `playingRef`. */
    private sealed trait PlayingRef extends Disposable[S#Tx] {
      def nodeOption: Option[AuralNode[S]]
    }

    private[this] object PlayingNone extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = ()
      def nodeOption = None
    }
    private final class PlayingNode(val node: AuralNode[S]) extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = {
        auralOutputs.foreach { case (_, view) =>
          view.stop()
        }
        auralAttrMap.foreach { case (_, view) =>
          view.stop()
        }
        node.dispose()
      }

      def nodeOption = Some(node)
    }
    private final class PlayingPrepare(val resources: List[AsyncResource[S]]) extends PlayingRef {
      def dispose()(implicit tx: S#Tx): Unit = resources.foreach(_.dispose())
      def nodeOption = None
    }

    // XXX TODO - perhaps `currentStateRef` and `playingRef` could be one thing?
    private[this] val currentStateRef     = Ref[AuralView.State](Stopped    )
    private[this] val targetStateRef      = Ref[TargetState    ](TargetStop )
    private[this] val playingRef          = Ref[PlayingRef     ](PlayingNone)

    final def typeID: Int = Proc.typeID

    final def state      (implicit tx: S#Tx): AuralView.State = currentStateRef()
    final def targetState(implicit tx: S#Tx): AuralView.State = targetStateRef ().completed

    private def state_=(value: AuralView.State)(implicit tx: S#Tx): Unit = {
      val old = currentStateRef.swap(value)
      if (value != old) {
        fire(value)
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def init(proc: Proc[S])(implicit tx: S#Tx): this.type = {
      _obj            = tx.newHandle(proc)
      val ugenInit    = UGB.init(proc)
      buildStateRef() = ugenInit

      observers ::= proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Proc.GraphChange(_)        => newSynthGraph()
          case Proc.OutputAdded  (output) => outputAdded(output)
          case Proc.OutputRemoved(output) => outputRemoved(output)
        }
      }
      val attr = proc.attr
      observers ::= attr.changed.react { implicit tx => upd => upd.changes.foreach {
        case Obj.AttrAdded   (key, value) => attrAdded  (key, value)
        case Obj.AttrRemoved (key, value) => attrRemoved(key, value)
        case Obj.AttrReplaced(key, before, now) =>
          attrRemoved(key, before)
          attrAdded  (key, now   )
      }}

      tryBuild()
      this
    }

    final def nodeOption(implicit tx: TxnLike): Option[NodeRef] = playingRef().nodeOption

    @inline
    private[this] def playOutputs(n: NodeRef)(implicit tx: S#Tx): Unit = {
      logA(s"playOutputs ${procCached()}")
      auralOutputs.foreach { case (_, view) =>
        view.play(n)
      }
    }

    private def newSynthGraph()(implicit tx: S#Tx): Unit = {
      logA(s"newSynthGraph ${procCached()}")

      if (state == Playing) stopForRebuild()

      disposeBuild()

      // then try to rebuild the stuff
      val ugenInit    = UGB.init(procCached())
      buildStateRef() = ugenInit
      tryBuild() // this will re-start the temporarily stopped views if possible
    }

    // ---- scan events ----

    private def outputAdded(output: Output[S])(implicit tx: S#Tx): Unit = {
      logA(s"outputAdded  to   ${procCached()} (${output.key})")
      val key = output.key
      outputBuses.get(key).foreach { bus =>
        val view = mkAuralOutput(output, bus)
        nodeOption.foreach(view.play)
      }
    }

    private def outputRemoved(output: Output[S])(implicit tx: S#Tx): Unit = {
      logA(s"outputRemoved from ${procCached()} (${output.key})")
      context.getAux[AuralOutput[S]](output.id).foreach(disposeAuralOutput)
//      val key = output.key
//      state.outputs.get(key).foreach { numCh =>
//        ... // XXX TODO - what was I thinking to do here?
//      }
    }

    @inline
    private[this] def disposeAuralOutput(view: AuralOutput[S])(implicit tx: S#Tx): Unit = {
      view.dispose() // this will call `context.removeAux`
      val exists = auralOutputs.remove(view.key)
      if (exists.isEmpty) throw new IllegalStateException(s"AuralOutput ${view.key} was not in map")
      ports(AuralObj.Proc.OutputRemoved(this, view))
    }

    // ---- attr events ----

    private def attrAdded(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      val st          = buildState
      val aKey        = UGB.AttributeKey(key)
      val rejected    = st.rejectedInputs.contains(aKey)
      val acceptedMap = st.acceptedInputs.getOrElse(aKey, Map.empty)
      val used        = rejected || acceptedMap.nonEmpty
      logA(s"AttrAdded   to   ${procCached()} ($key) - used? $used")
      if (!used) return

      val view = mkAuralAttribute(key, value)
      st match {
        case _: Complete[S] =>
          acceptedMap.foreach {
            case (_, UGB.Input.Scalar.Value(numChannels)) =>
              playingRef() match {
                case p: PlayingNode =>
                  val nr      = p.node
                  val target  = AuralAttribute.Target(nodeRef = nr, key, Bus.audio(server, numChannels = numChannels))
                  val trNew   = nr.shiftTo(sched.time)
                  view.play(timeRef = trNew, target = target)
                case _ =>
              }

            case _ =>
          }

        case _: Incomplete[S] =>
          assert(acceptedMap.isEmpty)
          // if (acceptedMap.isEmpty) {  // rejected
            // give it another incremental try
            tryBuild()
//          } else acceptedMap.foreach { case (input, valueBefore) =>
//            // if the request value changes or the
//            // new request is rejected, we have to
//            // rebuild the whole thing
//            try {
//              val valueNow = requestInput[input.Value](input, st0)
//              if (valueNow != valueBefore) newSynthGraph()
//            } catch {
//              case MissingIn(_) => newSynthGraph()
//            }
//          }

        case _ =>
      }
    }

    private def attrRemoved(key: String, value: Obj[S])(implicit tx: S#Tx): Unit = {
      logA(s"AttrRemoved from ${procCached()} ($key)")
      auralAttrMap.remove(key).foreach { view =>
        ports(AuralObj.Proc.AttrRemoved(this, view))
        view.dispose()
      }
    }

    // ----

    // creates an `AuralOutput` and registers it with the aural context.
    private def addUsedOutput(key: String, numChannels: Int)(implicit tx: S#Tx): Unit = {
      val outputs = procCached().outputs
      val bus     = Bus.audio(server, numChannels = numChannels) // mkBus(key, numChannels)
      outputBuses.put(key, bus).foreach(_ => throw new IllegalStateException(s"Output bus for $key already defined"))
      outputs.get(key).foreach { output =>
        mkAuralOutput(output, bus)
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: S#Tx): Unit = {
      freePlayingRef()
      observers.foreach(_.dispose())
      disposeBuild()
    }

    // does _not_ dispose playingRef
    private def disposeBuild()(implicit tx: S#Tx): Unit = {
      auralOutputs.foreach { case (_, view) =>
        ports(AuralObj.Proc.OutputRemoved(this, view))
        view.dispose()
      }
      auralOutputs.clear()
      outputBuses .clear()

      auralAttrMap.foreach { case (_, view) =>
        ports(AuralObj.Proc.AttrRemoved(this, view))
        view.dispose()
      }
      auralAttrMap.clear()

      genViewMap.foreach { case (_, view) =>
        view.dispose()
      }
      genViewMap.clear()
    }

    protected final def buildState(implicit tx: S#Tx): UGB.State[S] = buildStateRef()

    protected final def addObserver(obs: Disposable[S#Tx]): Unit = observers ::= obs

    /* If the ugen graph is incomplete, tries to (incrementally)
     * build it. Calls `buildAdvanced` with the old and new
     * state then.
     */
    protected final def tryBuild()(implicit tx: S#Tx): Unit = {
      buildState match {
        case s0: Incomplete[S] =>
          logA(s"try build ${procCached()} - ${procCached().name}")
          val s1          = invokeRetry(s0)
          buildStateRef() = s1
          buildAdvanced(before = s0, now = s1)

        case _: Complete[S] => // nada
      }
    }

    /** Sub-classes may override this to provide additional context,
      * but should then call `super.invokeRetry`.
      */
    protected def invokeRetry(state: UGB.Incomplete[S])(implicit tx: S#Tx): UGB.State[S] =
      state.retry(this)

    /* Called after invoking `retry` on the ugen graph builder.
     * The methods looks for new scan-ins and scan-outs used by
     * the ugen graph, and creates aural-scans for them, or
     * at least the bus-proxies if no matching entries exist
     * in the proc's `scans` dictionary.
     *
     * If the now-state indicates that the ugen-graph is complete,
     * it calls `play` on the proc-views whose target-state is to play.
     */
    private def buildAdvanced(before: UGB.State[S], now: UGB.State[S])(implicit tx: S#Tx): Unit = {
      // XXX TODO XXX TODO XXX TODO
      // this code is now partly wrong, because
      // we do not create the UGen graphs incrementally
      // any longer.

      // handle newly rejected inputs
      if (now.rejectedInputs.isEmpty) {
        logA(s"buildAdvanced ${procCached()}; complete? ${now.isComplete}")
      } else {
        logA(s"buildAdvanced ${procCached()}; rejectedInputs = ${now.rejectedInputs.mkString(",")}")
      }

      // handle newly visible outputs
      if (before.outputs ne now.outputs) {
        // detect which new outputs have been determined in the last iteration
        // (newOuts is a map from `name: String` to `numChannels Int`)
        val newOuts = now.outputs.filterNot {
          case (key, _) => before.outputs.contains(key)
        }
        logA(s"...newOuts = ${newOuts.mkString(",")}")

        newOuts.foreach { case (key, numCh) =>
          addUsedOutput(key, numCh)
        }
      }

      if (now.isComplete && targetState == Playing) playAfterRebuild()
    }

    /* Creates a new aural output */
    private def mkAuralOutput(output: Output[S], bus: AudioBus)(implicit tx: S#Tx): AuralOutput.Owned[S] = {
      val view  = AuralOutput(view = this, output = output, bus = bus)
      // this is done by the `AuralOutput` constructor:
      // context.putAux[AuralOutput[S]](output.id, view)
      val old = auralOutputs.put(output.key, view)
      if (old.isDefined) throw new IllegalStateException(s"AuralOutput already exists for ${output.key}")
      ports(AuralObj.Proc.OutputAdded(this, view))
      view
    }

    private def mkAuralAttribute(key: String, value: Obj[S])(implicit tx: S#Tx): AuralAttribute[S] =
      auralAttrMap.get(key).getOrElse {
        val view = AuralAttribute(key, value, this)
        auralAttrMap.put(key, view)
        ports(AuralObj.Proc.AttrAdded(this, view))
        view
      }

    // AuralAttribute.Observer
    final def attrNumChannelsChanged(attr: AuralAttribute[S])(implicit tx: S#Tx): Unit = {
      val aKey = UGB.AttributeKey(attr.key)
      if (buildState.rejectedInputs.contains(aKey)) tryBuild()
    }

    private def genComplete(key: String)(implicit tx: S#Tx): Unit = {
      val st          = buildState
      val aKey        = UGB.AttributeKey(key)
      val rejected    = st.rejectedInputs.contains(aKey)
      val acceptedMap = st.acceptedInputs.getOrElse(aKey, Map.empty)
      logA(s"genComplete to ${procCached()} ($key) - rejected? $rejected")
      if (!rejected || acceptedMap.nonEmpty) return

      st match {
        case _: Incomplete[S] =>
          tryBuild()
        case _ =>
      }
    }

    final protected def mkGenView(a: Gen[S], key: String)(implicit tx: S#Tx): GenView[S] =
      genViewMap.get(key).getOrElse {
        import context.gen
        val view  = GenView(a)
        val obs   = view.react { implicit tx => state => if (state.isComplete) genComplete(key) }
        val res   = new ObservedGenView(view, obs)
        genViewMap.put(key, res)
        res
      } .gen

    /** Sub-classes may override this if invoking the super-method.
      * The `async` field of the return value is not used but overwritten
      * by the calling instance. It can thus be left at an arbitrary value.
      */
    protected def requestInputBuffer(key: String, value: Obj[S])(implicit tx: S#Tx): UGB.Input.Buffer.Value = value match {
      case a: DoubleVector[S] =>
        val v = a.value   // XXX TODO: would be better to write a.peer.size.value
        UGB.Input.Buffer.Value(numFrames = v.size.toLong, numChannels = 1, async = false)

      case a: AudioCue.Obj[S] =>
        // val spec = a.spec
        val spec = a.value.spec
        UGB.Input.Buffer.Value(numFrames = spec.numFrames, numChannels = spec.numChannels, async = false)

      case a: Gen[S] =>
        // bloody hell --- what should we return now?
        // we should look at the future, if it is immediately
        // successful; if not, we should throw `MissingIn` and
        // trace the completion of the async build process
        val genView   = mkGenView(a, key)
        val tryValue  = genView.value.getOrElse(throw MissingIn(UGB.AttributeKey(key)))
        val newValue  = tryValue.get
        requestInputBuffer(key, newValue) // XXX TODO --- there is no sensible value for `key` now

      case _ =>
        throw new IllegalStateException(s"Unsupported input attribute buffer source $value")
    }

    /** Sub-classes may override this if invoking the super-method. */
    def requestInput[Res](in: UGB.Input { type Value = Res }, st: UGB.Requester[S])
                         (implicit tx: S#Tx): Res = in match {
      case i: UGB.Input.Scalar =>
        val procObj   = procCached()
        val aKey      = i.name
        val valueOpt  = procObj.attr.get(aKey)

        def auralChans(value: Obj[S]): Int = {
          val view = mkAuralAttribute(aKey, value)
          view.preferredNumChannels
        }

        val found: Int = valueOpt.fold(-1) {
          case a: Gen[S] =>
            val genView   = mkGenView(a, aKey)
            genView.value.fold(-1) { tr =>
              val value = tr.get
              auralChans(value)
            }

          case value =>
            auralChans(value)
        }

        import i.{defaultNumChannels => defNum, requiredNumChannels => reqNum}
        if ((found < 0 && i.defaultNumChannels < 0) || (found >= 0 && reqNum >= 0 && found != reqNum)) {
          // throw new IllegalStateException(s"Attribute ${i.name} requires $reqNum channels (found $found)")
          throw MissingIn(i.key)
        }
        val res = if (found >= 0) found else if (reqNum >= 0) reqNum else defNum
        UGB.Input.Scalar.Value(res)

      case i: UGB.Input.Stream =>
        val procObj       = procCached()
        val aKey          = i.name
        val valueOpt      = procObj.attr.get(aKey)
        val value0        = valueOpt.fold(simpleInputStreamValue(-1))(value => requestAttrStreamValue(aKey, value))
        val chansUnknown  = value0.numChannels < 0
        if (chansUnknown && !i.spec.isEmpty) throw MissingIn(i.key)
        val value1        = if (chansUnknown) value0.copy(numChannels = 1) else value0 // simply default to 1
        val newSpecs0     = List.empty[UGB.Input.Stream.Spec]
//        st.acceptedInputs.get(i.key) match {
//          case Some((_, v: UGB.Input.Stream.Value))   => v.specs
//          case _                                      => Nil
//        }
        val newSpecs      = if (i.spec.isEmpty) newSpecs0 else {
          i.spec :: newSpecs0
        }
        val value2        = if (newSpecs.isEmpty) value1 else value1.copy(specs = newSpecs)
        value2

      case i: UGB.Input.Buffer =>
        val procObj   = procCached()
        val aKey      = i.name
        val attrValue = procObj.attr.get(aKey).getOrElse(throw MissingIn(i.key))
        val res0      = requestInputBuffer(aKey, attrValue)
        // larger files are asynchronously prepared, smaller ones read on the fly
        val async     = res0.numSamples > UGB.Input.Buffer.AsyncThreshold   // XXX TODO - that threshold should be configurable
        if (async == res0.async) res0 else res0.copy(async = async)

      case i: UGB.Input.Attribute =>
        val procObj = procCached()
        // WARNING: Scala compiler bug, cannot use `collect` with
        // `PartialFunction` here, only total function works.
        val opt: Option[Any] = procObj.attr.get(i.name).flatMap {
          case x: Expr[S, _] => Some(x.value)
          case _ => None
        }
        UGB.Input.Attribute.Value(opt)

      case i: UGB.Input.BufferOut => UGB.Unit
      case    UGB.Input.StopSelf  => UGB.Unit
      case i: UGB.Input.Action    => UGB.Input.Action .Value
      case i: UGB.Input.DiskOut   => UGB.Input.DiskOut.Value(i.numChannels)
      case i: UGB.Input.BufferGen => UGB.Input.BufferGen.Value(st.allocUniqueID())

      case _ => throw new IllegalStateException(s"Unsupported input request $in")
    }

    private def getOutputBus(key: String)(implicit tx: S#Tx): Option[AudioBus] =
      outputBuses.get(key)

    final protected def procCached()(implicit tx: S#Tx): Proc[S] = {
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj()
        procLoc.set(proc)
        proc
      }
    }

    @inline
    private[this] def getAuralOutput(output: Output[S])(implicit tx: S#Tx): Option[AuralOutput[S]] =
      context.getAux[AuralOutput[S]](output.id)

    protected final def simpleInputStreamValue(numChannels: Int): UGB.Input.Stream.Value =
      UGB.Input.Stream.Value(numChannels = numChannels, sampleRate = server.sampleRate, specs = Nil)

    /** Sub-classes may override this if invoking the super-method. */
    protected def requestAttrStreamValue(key: String, value: Obj[S])
                                        (implicit tx: S#Tx): UGB.Input.Stream.Value = {
      value match {
        case a: DoubleVector[S] =>
          simpleInputStreamValue(a.value.size) // XXX TODO: would be better to write a.peer.size.value
        case a: AudioCue.Obj[S] =>
          val spec = a.value.spec
          UGB.Input.Stream.Value(numChannels = spec.numChannels, sampleRate = spec.sampleRate, specs = Nil)
        case _: FadeSpec.Obj[S] => simpleInputStreamValue(4)
        case a: Output[S] =>
          simpleInputStreamValue(getAuralOutput(a).fold(-1)(_.bus.numChannels))
        case a: Gen[S] =>
          val genView = mkGenView(a, key)
          genView.value.fold(simpleInputStreamValue(-1)) { tryValue =>
            val newValue = tryValue.get
            requestAttrStreamValue(key, newValue) // XXX TODO --- there is no sensible value for `key` now
          }

        case a => sys.error(s"Cannot use attribute $a as an audio stream")
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAttrStreamInput(nr: NodeRef.Full[S], timeRef: TimeRef, key: String,
                                       info: UGB.Input.Stream.Spec, idx: Int,
                                       bufSize: Int, value: Obj[S])
                                      (implicit tx: S#Tx): BufferAndGain = value match {
      case a: AudioCue.Obj[S] =>
        val audioVal  = a.value
        val spec      = audioVal.spec
        val path      = audioVal.artifact.getAbsolutePath
        val _gain     = audioVal.gain
        val offsetT   = ((audioVal.offset + timeRef.offset) * spec.sampleRate / SampleRate + 0.5).toLong
        val _buf      = if (info.isNative) {
          // XXX DIRTY HACK
          val offset1 = if (key.contains("!rnd")) {
            val fOffset = audioVal.fileOffset
            fOffset + (math.random * (spec.numFrames - fOffset)).toLong
          } else {
            offsetT
          }
          // println(s"OFFSET = $offset1")
          Buffer.diskIn(server)(
            path          = path,
            startFrame    = offset1,
            numFrames     = bufSize,
            numChannels   = spec.numChannels
          )
        } else {
          val __buf = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
          val trig = new StreamBuffer(key = key, idx = idx, synth = nr.node, buf = __buf, path = path,
            fileFrames = spec.numFrames, interp = info.interp, startFrame = offsetT, loop = false,
            resetFrame = offsetT)
          nr.addUser(trig)
          __buf
        }
        new BufferAndGain(_buf, _gain.toFloat)

      case _: Gen[S] =>
        val valueOpt: Option[Obj[S]] = for {
          observed <- genViewMap.get(key)
          tryOpt   <- observed.gen.value
          value    <- tryOpt.toOption
        } yield value

        val newValue = valueOpt.getOrElse(sys.error(s"Missing attribute $key for stream content"))
        buildAttrStreamInput(nr, timeRef, key = key, info = info, idx = idx, bufSize = bufSize, value = newValue)

      case a => sys.error(s"Cannot use attribute $a as an audio stream")
    }

    /** Sub-classes may override this if invoking the super-method.
      * If the value is incompatible with the assigned `value` and rebuilding the
      * synth-graph would alleviate that problem, a `MissingIn` should be thrown.
      * If the problem does not change in terms of the re-evaluation of the
      * synth-graph, a different generic exception must be thrown to avoid
      * an infinite loop.
      */
    protected def buildAttrInput(nr: NodeRef.Full[S], timeRef: TimeRef, key: String, value: UGB.Value)
                      (implicit tx: S#Tx): Unit = {
      value match {
        case UGB.Input.Scalar.Value(numChannels) =>  // --------------------- scalar
          auralAttrMap.get(key).foreach { a =>
            val target = AuralAttribute.Target(nr, key, Bus.audio(server, numChannels))
            a.play(timeRef = timeRef, target = target)
          }

        case UGB.Input.Stream.Value(_ /* numChannels */, _, specs) =>  // ------------------ streaming
          val infoSeq = if (specs.isEmpty) UGB.Input.Stream.EmptySpec :: Nil else specs

          infoSeq.zipWithIndex.foreach { case (info, idx) =>
            val ctlName     = graph.impl.Stream.controlName(key, idx)
            val bufSize     = if (info.isEmpty) server.config.blockSize else {
              val maxSpeed  = if (info.maxSpeed <= 0.0) 1.0 else info.maxSpeed
              val bufDur    = 1.5 * maxSpeed
              val minSz     = (2 * server.config.blockSize * math.max(1.0, maxSpeed)).toInt
              val bestSz    = math.max(minSz, (bufDur * server.sampleRate).toInt)
              import numbers.Implicits._
              val bestSzHi  = bestSz.nextPowerOfTwo
              val bestSzLo  = bestSzHi >> 1
              if (bestSzHi.toDouble/bestSz < bestSz.toDouble/bestSzLo) bestSzHi else bestSzLo
            }
            val bufAndGain: BufferAndGain = procCached().attr.get(key).fold {
              // DiskIn and VDiskIn are fine with an empty non-streaming buffer, as far as I can tell...
              // So instead of aborting when the attribute is not set, fall back to zero
              val _buf = Buffer(server)(numFrames = bufSize, numChannels = 1)
              new BufferAndGain(_buf, 0f)
            } { value =>
              buildAttrStreamInput(nr = nr, timeRef = timeRef, key = key,
                info = info, idx = idx, bufSize = bufSize, value = value)
            }
            nr.addControl(ctlName -> Seq[Float](bufAndGain.buf.id, bufAndGain.gain): ControlSet)
            val late = Buffer.disposeWithNode(bufAndGain.buf, nr)
            nr.addResource(late)
          }

        case UGB.Input.Buffer.Value(_ /* numFr */, _ /* numCh */, false) =>   // ----------------------- random access buffer
          val rb = procCached().attr.get(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for buffer content")
          } {
            case a: AudioCue.Obj[S] =>
              val audioVal  = a.value
              val spec      = audioVal.spec
              val path      = audioVal.artifact.getAbsolutePath
              val offset    = audioVal.fileOffset
              // XXX TODO - for now, gain is ignored.
              // one might add an auxiliary control proxy e.g. Buffer(...).gain
              // val _gain     = audioElem.gain    .value
              if (spec.numFrames > 0x3FFFFFFF)
                sys.error(s"File too large for in-memory buffer: $path (${spec.numFrames} frames)")
              val bufSize   = spec.numFrames.toInt
              val _buf      = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
              _buf.read(path = path, fileStartFrame = offset)
              _buf

            case dv: DoubleVector[S] =>
              val values    = dv.value.map(_.toFloat)
              val bufSize   = values.size
              val _buf      = Buffer(server)(numFrames = bufSize, numChannels = 1)
              _buf.setn(values)
              _buf

            case a => sys.error(s"Cannot use attribute $a as a buffer content")
          }
          val ctlName    = graph.Buffer.controlName(key)
          nr.addControl(ctlName -> rb.id)
          val late = Buffer.disposeWithNode(rb, nr)
          nr.addResource(late)

        case UGB.Input.Action.Value =>   // ----------------------- action
          val resp = new ActionResponder(objH = obj /* tx.newHandle(nr.obj) */, key = key, synth = nr.node)
          nr.addUser(resp)

        case UGB.Input.DiskOut.Value(numCh) =>
          val rb = procCached().attr.get(key).fold[Buffer] {
            sys.error(s"Missing attribute $key for disk-out artifact")
          } {
            case a: Artifact[S] =>
              val artifact  = a
              val f         = artifact.value.absolute
              val ext       = f.ext.toLowerCase
              val tpe       = AudioFileType.writable.find(_.extensions.contains(ext)).getOrElse(AudioFileType.AIFF)
              val _buf      = Buffer.diskOut(server)(path = f.path, fileType = tpe, numChannels = numCh)
              _buf

            case a => sys.error(s"Cannot use attribute $a as an artifact")
          }
          val ctlName    = graph.DiskOut.controlName(key)
          nr.addControl(ctlName -> rb.id)
          val late = Buffer.disposeWithNode(rb, nr)
          nr.addResource(late)

        case _: UGB.Input.Attribute.Value =>

        case _ =>
          throw new IllegalStateException(s"Unsupported input attribute request $value")
      }
    }

    final def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit = {
      targetStateRef() = TargetPrepared
      // XXX TODO
    }

    final def play(timeRef: TimeRef.Option, unit: Unit)(implicit tx: S#Tx): Unit = {
      val tr  = timeRef.force
      val ts  = TargetPlaying(sched.time, tr)
      targetStateRef() = ts
      buildState match {
        case s: UGB.Complete[S] =>
          state match {
            case Stopped   => prepareAndLaunch(s, tr)
            case Prepared  => launch          (s, tr)
            case _ =>
          }

        case _ =>
          // state = Preparing
      }
    }

    // same as `play` but reusing previous `timeRef`
    private def playAfterRebuild()(implicit tx: S#Tx): Unit = {
      // if (state != Stopped) return
      if (state == Playing) return

      (buildState, targetStateRef()) match {
        case (s: UGB.Complete[S], tp: TargetPlaying) =>
          prepareAndLaunch(s, tp.shiftTo(sched.time))
        case _ =>
      }
    }

    final def stop(/* time: Long */)(implicit tx: S#Tx): Unit = {
      targetStateRef() = TargetStop
      stopForRebuild()
    }

    // same as `stop` but not touching target state
    private def stopForRebuild()(implicit tx: S#Tx): Unit = {
      freePlayingRef()
      state = Stopped
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildAsyncInput(b: AsyncProcBuilder[S], keyW: UGB.Key, value: UGB.Value)
                                 (implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key) => buildAsyncAttrInput(b, key, value)
      case _                     => throw new IllegalStateException(s"Unsupported async input request $keyW")
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildSyncInput(nr: NodeRef.Full[S], timeRef: TimeRef, keyW: UGB.Key, value: UGB.Value)
                                (implicit tx: S#Tx): Unit = keyW match {
      case UGB.AttributeKey(key) =>
        buildAttrInput(nr, timeRef, key, value)

      case UGB.Input.BufferOut(artifactKey, actionKey, numFrames, numChannels) =>
        val attr    = procCached().attr
        val art     = attr.$[Artifact](artifactKey).getOrElse {
          sys.error(s"Missing attribute $artifactKey for buffer-out artifact")
        }
        val artV    = art.value
//        val act     = attr.$[Action](actionKey).getOrElse {
//          sys.error(s"Missing attribute $actionKey for buffer-out action")
//        }
        val rb      = Buffer(server)(numFrames = numFrames, numChannels = numChannels)
        val ctlName = graph.BufferOut.controlName(artifact = artifactKey, action = actionKey)
        nr.addControl(ctlName -> rb.id)
        val late = Buffer.writeWithNode(rb, nr, artV) {
          cursor.step { implicit tx =>
            rb.dispose()
            val invoker = procCached()
            invoker.attr.$[Action](actionKey).foreach { action =>
              val universe = Action.Universe(action, context.workspaceHandle,
                invoker = Some(invoker) /* , values = values */)
              action.execute(universe)
            }
          }
        } (SoundProcesses.executionContext)
        nr.addResource(late)

      case UGB.Input.StopSelf =>
        val resp = new StopSelfResponder[S](view = impl, synth = nr.node)
        nr.addUser(resp)

      case bg: UGB.Input.BufferGen =>
        val UGB.Input.BufferGen.Value(id) = value
        val rb      = Buffer(nr.server)(numFrames = bg.numFrames, numChannels = bg.numChannels)
        rb.gen(bg.cmd)
        val ctlName = graph.BufferGen.controlName(id)
        nr.addControl(ctlName -> rb.id)
        val late = Buffer.disposeWithNode(rb, nr)
        nr.addResource(late)

      case _ =>
        throw new IllegalStateException(s"Unsupported input request $keyW")
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAsyncAttrBufferInput(b: AsyncProcBuilder[S], key: String, value: Obj[S])
                                           (implicit tx: S#Tx): Unit = value match {
      case a: AudioCue.Obj[S] =>
        val audioVal  = a.value
        val spec      = audioVal.spec
        val f         = audioVal.artifact
        val offset    = audioVal.fileOffset
        // XXX TODO - for now, gain is ignored.
        // one might add an auxiliary control proxy e.g. Buffer(...).gain
        // val _gain     = audioElem.gain    .value
        if (spec.numFrames > 0x3FFFFFFF)
          sys.error(s"File too large for in-memory buffer: $f (${spec.numFrames} frames)")
        val bufSize   = spec.numFrames.toInt
        val buf       = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
        val cfg       = BufferPrepare.Config(f = f, spec = spec, offset = offset, buf = buf, key = key)
        b.resources ::= BufferPrepare[S](cfg)

      case _: Gen[S] =>
        val valueOpt: Option[Obj[S]] = for {
          observed <- genViewMap.get(key)
          tryOpt   <- observed.gen.value
          value    <- tryOpt.toOption
        } yield value

        val newValue = valueOpt.getOrElse(sys.error(s"Missing attribute $key for buffer content"))
        buildAsyncAttrBufferInput(b, key, newValue)

      case a => sys.error(s"Cannot use attribute $a as a buffer content")
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAsyncAttrInput(b: AsyncProcBuilder[S], key: String, value: UGB.Value)
                                     (implicit tx: S#Tx): Unit = value match {
      case UGB.Input.Buffer.Value(_ /* numFr */, _ /* numCh */, true) =>   // ----------------------- random access buffer
        val bValue = b.obj.attr.get(key).getOrElse(sys.error(s"Missing attribute $key for buffer content"))
        buildAsyncAttrBufferInput(b, key, bValue)

      case _ =>
        throw new IllegalStateException(s"Unsupported input attribute request $value")
    }

    // ---- asynchronous preparation ----
    private def prepareAndLaunch(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val p = procCached()
      logA(s"begin prepare $p (${hashCode.toHexString})")

      val b = new AsyncProcBuilder(p)
      ugen.acceptedInputs.foreach { case (key, map) =>
        map.foreach { case (_, value) =>
          if (value.async) buildAsyncInput(b, key, value)
        }
      }
      val res   = b.resources
      val done  = res.isEmpty
      if (done) {
        freePlayingRef()
        prepared(ugen)
      } else {
        val prep = setPlayingPrepare(res)
        tx.afterCommit {
          import SoundProcesses.executionContext
          val reduced = Future.reduce(res)((_, _) => ()) // reduceLeft requires Scala 2.12
          reduced.foreach { _ =>
            cursor.step { implicit tx =>
              if (playingRef() == prep) {
                prepared(ugen)
              }
            }
          }
        }
      }
    }

    private def prepared(ugen: UGB.Complete[S])(implicit tx: S#Tx): Unit =
      targetStateRef() match {
        case tp: TargetPlaying =>
          launch(ugen, tp.shiftTo(sched.time)) // XXX TODO - yes or no, shift time?
        case _ =>
          state = Prepared
      }

    // ---- synchronous preparation ----
    protected def launch(ugen: UGB.Complete[S], timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val p = procCached()
      logA(s"begin launch  $p (${hashCode.toHexString})")

      val ubRes         = ugen.result
      val nameHint      = p.attr.$[StringObj](ObjKeys.attrName).map(_.value)
      val builder       = AuralNode[S](timeRef, sched.time, ubRes, server, nameHint = nameHint)
      val synth         = builder.node

      // "consume" prepared state
      playingRef.swap(PlayingNone) match {
        case prep: PlayingPrepare =>
          prep.resources.foreach { resource =>
            resource.install(builder)
          }
        case _ =>
      }

      // XXX TODO - it would be nicer if these were added optionally
      if (timeRef.frame  != 0) builder.addControl(graph.Time    .key -> (timeRef.frame  / SampleRate))
      if (timeRef.offset != 0) builder.addControl(graph.Offset  .key -> (timeRef.offset / SampleRate))
      timeRef.span match {
        case Span(start, stop) =>
          builder.addControl(graph.Duration.key -> ((stop - start) / SampleRate))
        case _ => // Double.PositiveInfinity
      }

      ugen.acceptedInputs.foreach { case (key, map) =>
        map.foreach { case (_, value) =>
          if (!value.async) buildSyncInput(builder, timeRef, key, value)
        }
      }

      // ---- handle output buses, and establish missing links to sinks ----
      ugen.outputs.foreach { case (key, _ /* numCh */) =>
        val bus    = getOutputBus(key) getOrElse sys.error(s"Scan bus $key not provided")
        logA(s"addOutputBus($key, $bus) (${hashCode.toHexString})")
        val res    = BusNodeSetter.writer(graph.ScanOut.controlName(key), bus, synth)
        builder.addUser(res)
      }

      val old       = playingRef.swap(new PlayingNode(builder))
      old.dispose()
      builder.play()
      playOutputs(builder)

      logA(s"launched $p -> $builder (${hashCode.toHexString})")
      state = Playing
    }

    private def setPlayingPrepare(resources: List[AsyncResource[S]])(implicit tx: S#Tx): PlayingPrepare = {
      val res = new PlayingPrepare(resources)
      val old = playingRef.swap(res)
      old.dispose()
      state = Preparing
      res
    }

    private def freePlayingRef()(implicit tx: S#Tx): Unit = {
      val old = playingRef.swap(PlayingNone)
      old.dispose()
    }
  }
}