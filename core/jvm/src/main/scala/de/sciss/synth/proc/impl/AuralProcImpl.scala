/*
 *  AuralProcImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.equal.Implicits._
import de.sciss.file._
import de.sciss.lucre.impl.ObservableImpl
import de.sciss.lucre.synth.{AudioBus, Buffer, Bus, BusNodeSetter, Executor, NodeRef, Server}
import de.sciss.lucre.{Artifact, Disposable, DoubleVector, ExprLike, IExpr, IntVector, Obj, Source, StringObj, Txn, TxnLike, synth}
import de.sciss.numbers
import de.sciss.span.Span
import de.sciss.synth.ControlSet
import de.sciss.synth.UGenSource.Vec
import de.sciss.audiofile.AudioFileType
import de.sciss.synth.proc.AuralObj.{TargetPlaying, TargetPrepared, TargetState, TargetStop}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.Runner.{Prepared, Preparing, Running, Stopped}
import de.sciss.synth.proc.TimeRef.SampleRate
import de.sciss.synth.proc.UGenGraphBuilder.{Complete, Incomplete, MissingIn}
import de.sciss.synth.proc.graph.impl.{ActionResponder, StopSelfResponder}
import de.sciss.synth.proc.{Action, AudioCue, AuralAttribute, AuralContext, AuralNode, AuralObj, AuralOutput, FadeSpec, Gen, GenView, ObjKeys, Proc, Runner, TimeRef, graph, UGenGraphBuilder => UGB, logAural => logA}

import scala.concurrent.Future
import scala.concurrent.stm.{Ref, TMap, TxnLocal}

// We proudly present: The horror object
object AuralProcImpl {
  def apply[T <: synth.Txn[T]](proc: Proc[T], attr: Runner.Attr[T])
                        (implicit tx: T, context: AuralContext[T]): AuralObj.Proc[T] = {
    val res = new Impl[T](attr)
    res.init(proc)
  }

  private final class ObservedGenView[T <: Txn[T]](val gen: GenView[T], obs: Disposable[T])
    extends Disposable[T] {

    def dispose()(implicit tx: T): Unit = {
      obs.dispose()
      gen.dispose()
    }
  }

  final class BufferAndGain(val buf: Buffer, val gain: Float)

  class Impl[T <: synth.Txn[T]](protected val runnerAttr: Runner.Attr[T])(implicit val context: AuralContext[T])
    extends AuralObj.Proc[T]
    with UGB.Context[T]
    with AuralAttribute.Observer[T]
    with ObservableImpl[T, Runner.State] { impl =>

    import Txn.peer
//    import context.{scheduler => sched}
//    import context.scheduler.cursor
    import context.universe.{cursor, scheduler => sched}

    private[this] val buildStateRef = Ref.make[UGB.State[T]]()

    // running attribute inputs
    private[this] val auralAttrMap  = TMap.empty[String, AuralAttribute[T]]
    private[this] val outputBuses   = TMap.empty[String, AudioBus]
    private[this] val auralOutputs  = TMap.empty[String, AuralOutput.Owned[T]]

    private[this] val genViewMap    = TMap.empty[String, ObservedGenView[T]]

    private[this] val procLoc       = TxnLocal[Proc[T]]() // cache-only purpose

    private[this] var observers     = List.empty[Disposable[T]]

    private[this] var _objH: Source[T, Proc[T]] = _

//    final def objH: stm.Source[T, Proc[T]] = _obj

    final def obj(implicit tx: T): Proc[T] = _objH()

    override def toString = s"AuralObj.Proc@${hashCode().toHexString}"

    final def server: Server = context.server

    object ports extends ObservableImpl[T, AuralObj.Proc.Update[T]] {
      def apply(update: AuralObj.Proc.Update[T])(implicit tx: T): Unit = fire(update)
    }

    def getAttr  (key: String)(implicit tx: T): Option[AuralAttribute[T]] = auralAttrMap.get(key)
    def getOutput(key: String)(implicit tx: T): Option[AuralOutput   [T]] = auralOutputs.get(key)

    /* The ongoing build aural node build process, as stored in `playingRef`. */
    private sealed trait PlayingRef extends Disposable[T] {
      def nodeOption: Option[AuralNode[T]]
    }

    private[this] object PlayingNone extends PlayingRef {
      def dispose()(implicit tx: T): Unit = ()
      def nodeOption: Option[AuralNode[T]] = None
    }
    private final class PlayingNode(val node: AuralNode[T]) extends PlayingRef {
      def dispose()(implicit tx: T): Unit = {
        auralOutputs.foreach { case (_, view) =>
          view.stop()
        }
        auralAttrMap.foreach { case (_, view) =>
          view.stop()
        }
        node.dispose()
      }

      def nodeOption: Option[AuralNode[T]] = Some(node)
    }
    private final class PlayingPrepare(val resources: List[AsyncResource[T]]) extends PlayingRef {
      def dispose()(implicit tx: T): Unit = resources.foreach(_.dispose())
      def nodeOption: Option[AuralNode[T]] = None
    }

    // XXX TODO - perhaps `currentStateRef` and `playingRef` could be one thing?
    private[this] val currentStateRef     = Ref[Runner.State](Stopped    )
    private[this] val targetStateRef      = Ref[TargetState    ](TargetStop )
    private[this] val playingRef          = Ref[PlayingRef     ](PlayingNone)

    final def tpe: Obj.Type = Proc

    final def state      (implicit tx: T): Runner.State = currentStateRef()
    final def targetState(implicit tx: T): Runner.State = targetStateRef ().completed

    private def state_=(value: Runner.State)(implicit tx: T): Unit = { // IntelliJ highlight bug -- it is used
      val old = currentStateRef.swap(value)
      if (value != old) {
        fire(value)
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def init(proc: Proc[T])(implicit tx: T): this.type = {
      _objH            = tx.newHandle(proc)
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
        case Obj.AttrRemoved (key, _ /*value*/) => attrRemoved(key /*, value*/)
        case Obj.AttrReplaced(key, _ /*before*/, now) =>
          attrRemoved(key /*, before*/)
          attrAdded  (key, now   )
      }}

      tryBuild()
      this
    }

    final def nodeOption(implicit tx: TxnLike): Option[NodeRef] = playingRef().nodeOption

    @inline
    private[this] def playOutputs(n: NodeRef)(implicit tx: T): Unit = {
      logA(s"playOutputs ${procCached()}")
      auralOutputs.foreach { case (_, view) =>
        view.play(n)
      }
    }

    private def newSynthGraph()(implicit tx: T): Unit = {
      logA(s"newSynthGraph ${procCached()}")

      if (state == Running) stopForRebuild()

      disposeBuild()

      // then try to rebuild the stuff
      val ugenInit    = UGB.init(procCached())
      buildStateRef() = ugenInit
      tryBuild() // this will re-start the temporarily stopped views if possible
    }

    // ---- scan events ----

    private def outputAdded(output: Proc.Output[T])(implicit tx: T): Unit = {
      logA(s"outputAdded  to   ${procCached()} (${output.key})")
      val key = output.key
      outputBuses.get(key).foreach { bus =>
        val view = mkAuralOutput(output, bus)
        nodeOption.foreach(view.play)
      }
    }

    private def outputRemoved(output: Proc.Output[T])(implicit tx: T): Unit = {
      logA(s"outputRemoved from ${procCached()} (${output.key})")
      context.getAux[AuralOutput[T]](output.id).foreach(disposeAuralOutput)
//      val key = output.key
//      state.outputs.get(key).foreach { numCh =>
//        ... // XXX TODO - what was I thinking to do here?
//      }
    }

    @inline
    private[this] def disposeAuralOutput(view: AuralOutput[T])(implicit tx: T): Unit = {
      view.dispose() // this will call `context.removeAux`
      val exists = auralOutputs.remove(view.key)
      if (exists.isEmpty) throw new IllegalStateException(s"AuralOutput ${view.key} was not in map")
      ports(AuralObj.Proc.OutputRemoved(this, view))
    }

    // ---- attr events ----

    private def attrAdded(key: String, value: Obj[T])(implicit tx: T): Unit = {
      val st          = buildState
      val aKey        = UGB.AttributeKey(key)
      val rejected    = st.rejectedInputs.contains(aKey)
      val acceptedMap = st.acceptedInputs.getOrElse(aKey, Map.empty)
      val used        = rejected || acceptedMap.nonEmpty
      logA(s"AttrAdded   to   ${procCached()} ($key) - used? $used")
      if (!used) return

      val view = mkAuralAttribute(key, value)
      st match {
        case _: Complete[T] =>
          acceptedMap.foreach {
            case (_, UGB.Input.Scalar.Value(numChannels)) =>
              playingRef() match {
                case p: PlayingNode =>
                  val nr      = p.node
                  val target  = AuralAttribute.Target(nodeRef = nr, key, Bus.audio(server, numChannels = numChannels))
                  val trNew   = nr.shiftTo(sched.time)
                  view.run(timeRef = trNew, target = target)
                case _ =>
              }

            case _ =>
          }

        case _: Incomplete[T] =>
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

    private def attrRemoved(key: String /*, value: Obj[T]*/)(implicit tx: T): Unit = {
      logA(s"AttrRemoved from ${procCached()} ($key)")
      auralAttrMap.remove(key).foreach { view =>
        ports(AuralObj.Proc.AttrRemoved(this, view))
        view.dispose()
      }
    }

    // ----

    // creates an `AuralOutput` and registers it with the aural context.
    private def addUsedOutput(key: String, numChannels: Int)(implicit tx: T): Unit = {
      val outputs = procCached().outputs
      val bus     = Bus.audio(server, numChannels = numChannels) // mkBus(key, numChannels)
      outputBuses.put(key, bus).foreach(_ => throw new IllegalStateException(s"Output bus for $key already defined"))
      outputs.get(key).foreach { output =>
        mkAuralOutput(output, bus)
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    def dispose()(implicit tx: T): Unit = {
      freePlayingRef()
      observers.foreach(_.dispose())
      disposeBuild()
    }

    /** Sub-classes may override this if invoking the super-method.
      * Note that this does ''not'' dispose `playingRef`.
      */
    protected def disposeBuild()(implicit tx: T): Unit = {
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

    protected final def buildState(implicit tx: T): UGB.State[T] = buildStateRef()

    protected final def addObserver(obs: Disposable[T]): Unit = observers ::= obs

    /* If the ugen graph is incomplete, tries to (incrementally)
     * build it. Calls `buildAdvanced` with the old and new
     * state then.
     */
    protected final def tryBuild()(implicit tx: T): Unit = {
      buildState match {
        case s0: Incomplete[T] =>
          logA(s"try build ${procCached()} - ${procCached().name}")
          val s1          = invokeRetry(s0)
          buildStateRef() = s1
          buildAdvanced(before = s0, now = s1)

        case _: Complete[T] => // nada
      }
    }

    /** Sub-classes may override this to provide additional context,
      * but should then call `super.invokeRetry`.
      */
    protected def invokeRetry(state: UGB.Incomplete[T])(implicit tx: T): UGB.State[T] =
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
    private def buildAdvanced(before: UGB.State[T], now: UGB.State[T])(implicit tx: T): Unit = {
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

      if (now.isComplete && targetState == Running) playAfterRebuild()
    }

    /* Creates a new aural output */
    private def mkAuralOutput(output: Proc.Output[T], bus: AudioBus)(implicit tx: T): AuralOutput.Owned[T] = {
      val view  = AuralOutput(view = this, output = output, bus = bus)
      // this is done by the `AuralOutput` constructor:
      // context.putAux[AuralOutput[T]](output.id, view)
      val old = auralOutputs.put(output.key, view)
      if (old.isDefined) throw new IllegalStateException(s"AuralOutput already exists for ${output.key}")
      ports(AuralObj.Proc.OutputAdded(this, view))
      view
    }

    private def mkAuralAttribute(key: String, value: Obj[T])(implicit tx: T): AuralAttribute[T] =
      auralAttrMap.get(key).getOrElse {
        val view = AuralAttribute(key, value, this)
        auralAttrMap.put(key, view)
        ports(AuralObj.Proc.AttrAdded(this, view))
        view
      }

    // XXX TODO DRY with `mkAuralAttribute`
    private def mkAuralExprLike(key: String, value: IExpr[T, _])(implicit tx: T): AuralAttribute[T] =
      auralAttrMap.get(key).getOrElse {
        val view = AuralAttribute.expr(key, value, this)
        auralAttrMap.put(key, view)
        ports(AuralObj.Proc.AttrAdded(this, view))
        view
      }

    // AuralAttribute.Observer
    final def attrNumChannelsChanged(attr: AuralAttribute[T])(implicit tx: T): Unit = {
      val aKey = UGB.AttributeKey(attr.key)
      if (buildState.rejectedInputs.contains(aKey)) tryBuild()
    }

    private def genComplete(key: String)(implicit tx: T): Unit = {
      val st          = buildState
      val aKey        = UGB.AttributeKey(key)
      val rejected    = st.rejectedInputs.contains(aKey)
      val acceptedMap = st.acceptedInputs.getOrElse(aKey, Map.empty)
      logA(s"genComplete to ${procCached()} ($key) - rejected? $rejected")
      if (!rejected || acceptedMap.nonEmpty) return

      st match {
        case _: Incomplete[T] =>
          tryBuild()
        case _ =>
      }
    }

    final protected def mkGenView(a: Gen[T], key: String)(implicit tx: T): GenView[T] =
      genViewMap.get(key).getOrElse {
//        import context.genContext
        import context.universe
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
    protected def requestInputBuffer(key: String, value: Obj[T])(implicit tx: T): UGB.Input.Buffer.Value = value match {
      case a: DoubleVector[T] =>
        val v = a.value   // XXX TODO: would be better to write a.peer.size.value
        UGB.Input.Buffer.Value(numFrames = v.size.toLong, numChannels = 1, async = false)

      case a: IntVector[T] =>
        val v = a.value   // XXX TODO: would be better to write a.peer.size.value
        UGB.Input.Buffer.Value(numFrames = v.size.toLong, numChannels = 1, async = false)

      case a: AudioCue.Obj[T] =>
        val cue       = a.value
        val spec      = cue.spec
        val numFrames = math.max(0L, spec.numFrames - cue.fileOffset)
        UGB.Input.Buffer.Value(numFrames = numFrames, numChannels = spec.numChannels, async = false)

      case a: Gen[T] =>
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

    private def requestInputBufferFromExpr(/*key: String,*/ ex: IExpr[T, _])(implicit tx: T): UGB.Input.Buffer.Value =
      ex.value match {
        case v: Vec[_] if v.forall(_.isInstanceOf[Double]) =>
          UGB.Input.Buffer.Value(numFrames = v.size.toLong, numChannels = 1, async = false)
        case v: Vec[_] if v.forall(_.isInstanceOf[Int]) =>
          UGB.Input.Buffer.Value(numFrames = v.size.toLong, numChannels = 1, async = false)

        case cue: AudioCue =>
          val spec      = cue.spec
          val numFrames = math.max(0L, spec.numFrames - cue.fileOffset)
          UGB.Input.Buffer.Value(numFrames = numFrames, numChannels = spec.numChannels, async = false)

        case _ =>
          throw new IllegalStateException(s"Unsupported input attribute buffer source $ex")
      }

      // XXX TODO the integration of runnerAttr is a bad hack
    /** Sub-classes may override this if invoking the super-method. */
    def requestInput[Res](in: UGB.Input { type Value = Res }, st: UGB.Requester[T])
                         (implicit tx: T): Res = in match {
      case i: UGB.Input.Scalar =>
        val aKey = i.name

        def tryRunnerAttr(): Int = {
          val valueOpt = runnerAttr.get(aKey)
          valueOpt.fold(-1) {
            case value: IExpr[T, _] =>
              val view = mkAuralExprLike(aKey, value)
              view.preferredNumChannels
            case _ => -1
          }
        }

        def tryObjAttr(): Int = {
          def channels(value: Obj[T]): Int = {
            val view = mkAuralAttribute(aKey, value)
            view.preferredNumChannels
          }

          val procObj   = procCached()
          val valueOpt  = procObj.attr.get(aKey)
          valueOpt.fold(-1) {
            case a: Gen[T] =>
              val genView = mkGenView(a, aKey)
              genView.value match {
                case Some(tr) =>
                  val value = tr.get
                  channels(value)
                case None => -1
              }

            case value =>
              channels(value)
          }
        }

        val found: Int = {
          val tmp = tryRunnerAttr()
          if (tmp == -1) tryObjAttr() else tmp
        }

        import i.{defaultNumChannels => defNum, requiredNumChannels => reqNum}
        if ((found < 0 && i.defaultNumChannels < 0) || (found >= 0 && reqNum >= 0 && found != reqNum)) {
          // throw new IllegalStateException(s"Attribute ${i.name} requires $reqNum channels (found $found)")
          throw MissingIn(i.key)
        }
        val res = if (found >= 0) found else if (reqNum >= 0) reqNum else defNum
        UGB.Input.Scalar.Value(res) // IntelliJ highlight bug

      case i: UGB.Input.Stream =>
        val aKey          = i.name
        val value0: UGB.Input.Stream.Value =
          runnerAttr.get(aKey) match {
            case Some(ex: IExpr[T, _]) =>
              requestAttrStreamValueFromExpr(/*aKey,*/ ex)
            case Some(a) =>
              sys.error(s"Cannot use attribute $a as an audio stream")
            case None =>
              val procObj       = procCached()
              val valueOpt      = procObj.attr.get(aKey)
              valueOpt match {
                case Some(value) =>
                  requestAttrStreamValue(aKey, value)
                case None =>
                  simpleInputStreamValue(-1)
              }
          }

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
        val aKey      = i.name
        val res0: UGB.Input.Buffer.Value = runnerAttr.get(aKey) match {
          case Some(ex: IExpr[T, _]) =>
            requestInputBufferFromExpr(/*aKey,*/ ex)
          case Some(a) =>
            throw new IllegalStateException(s"Unsupported input attribute buffer source $a")
          case None =>
            val procObj   = procCached()
            val attrValue = procObj.attr.get(aKey).getOrElse(throw MissingIn(i.key))
            requestInputBuffer(aKey, attrValue)
        }
        // larger files are asynchronously prepared, smaller ones read on the fly
        val async = res0.numSamples > UGB.Input.Buffer.AsyncThreshold   // XXX TODO - that threshold should be configurable
        if (async == res0.async) res0 else res0.copy(async = async)

      case i: UGB.Input.Attribute =>
        val aKey      = i.name
        val valueOpt  = runnerAttr.get(aKey).orElse {
          val procObj   = procCached()
          procObj.attr.get(aKey)
        }

        val opt: Option[Any] = valueOpt match {
          case Some(x: ExprLike[T, _])  => Some(x.value)
          case _                        => None
        }
        UGB.Input.Attribute.Value(opt)

      case _: UGB.Input.BufferOut => UGB.Unit
      case    UGB.Input.StopSelf  => UGB.Unit
      case _: UGB.Input.Action    => UGB.Input.Action .Value
      case i: UGB.Input.DiskOut   => UGB.Input.DiskOut.Value(i.numChannels)
      case _: UGB.Input.BufferGen => UGB.Input.BufferGen.Value(st.allocUniqueId())

      case _ => throw new IllegalStateException(s"Unsupported input request $in")
    }

    private def getOutputBus(key: String)(implicit tx: T): Option[AudioBus] =
      outputBuses.get(key)

    final protected def procCached()(implicit tx: T): Proc[T] = {
      if (procLoc.isInitialized) procLoc.get
      else {
        val proc = obj
        procLoc.set(proc)
        proc
      }
    }

    @inline
    private[this] def getAuralOutput(output: Proc.Output[T])(implicit tx: T): Option[AuralOutput[T]] =
      context.getAux[AuralOutput[T]](output.id)

    protected final def simpleInputStreamValue(numChannels: Int): UGB.Input.Stream.Value =
      UGB.Input.Stream.Value(numChannels = numChannels, sampleRate = server.sampleRate, specs = Nil)

    /** Sub-classes may override this if invoking the super-method. */
    protected def requestAttrStreamValue(key: String, value: Obj[T])
                                        (implicit tx: T): UGB.Input.Stream.Value = {
      value match {
        case a: DoubleVector[T] =>
          simpleInputStreamValue(a.value.size) // XXX TODO: would be better to write a.peer.size.value
        case a: IntVector[T] =>
          simpleInputStreamValue(a.value.size) // XXX TODO: would be better to write a.peer.size.value
        case a: AudioCue.Obj[T] =>
          val spec = a.value.spec
          UGB.Input.Stream.Value(numChannels = spec.numChannels, sampleRate = spec.sampleRate, specs = Nil)
        case _: FadeSpec.Obj[T] => simpleInputStreamValue(4)
        case a: Proc.Output[T] =>
          val numCh = getAuralOutput(a) match {
            case Some(out)  => out.bus.numChannels
            case None       => -1
          }
          simpleInputStreamValue(numCh)
        case a: Gen[T] =>
          val genView = mkGenView(a, key)
          genView.value match {
            case Some(tryValue) =>
              val newValue = tryValue.get
              requestAttrStreamValue(key, newValue) // XXX TODO --- there is no sensible value for `key` now
            case None =>
              simpleInputStreamValue(-1)
          }

        case a => sys.error(s"Cannot use attribute $a as an audio stream")
      }
    }

    private def requestAttrStreamValueFromExpr(/*key: String,*/ ex: IExpr[T, _])
                                        (implicit tx: T): UGB.Input.Stream.Value = {
      ex.value match {
        case sq: Vec[_] if sq.forall(_.isInstanceOf[Double]) =>
          simpleInputStreamValue(sq.size)
        case sq: Vec[_] if sq.forall(_.isInstanceOf[Int]) =>
          simpleInputStreamValue(sq.size)
        case a: AudioCue =>
          val spec = a.spec
          UGB.Input.Stream.Value(numChannels = spec.numChannels, sampleRate = spec.sampleRate, specs = Nil)
        case _: FadeSpec => simpleInputStreamValue(4)

        case a => sys.error(s"Cannot use attribute $a as an audio stream")
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAttrStreamInput(nr: NodeRef.Full[T], timeRef: TimeRef, key: String,
                                       info: UGB.Input.Stream.Spec, idx: Int, bufSize: Int)
                                      (implicit tx: T): BufferAndGain = {
      runnerAttr.get(key) match {
        case Some(ex: IExpr[T, _]) =>
          buildAttrStreamInputFromExpr(nr = nr, timeRef = timeRef, key = key,
            info = info, idx = idx, bufSize = bufSize, ex = ex)
        case Some(a) =>
          sys.error(s"Cannot use attribute $a as an audio stream")
        case None =>
          procCached().attr.get(key) match {
            case Some(value) =>
              buildAttrStreamInput(nr = nr, timeRef = timeRef, key = key,
                info = info, idx = idx, bufSize = bufSize, value = value)
            case None =>
              // DiskIn and VDiskIn are fine with an empty non-streaming buffer, as far as I can tell...
              // So instead of aborting when the attribute is not set, fall back to zero
              val _buf = Buffer(server)(numFrames = bufSize, numChannels = 1)
              new BufferAndGain(_buf, 0f)
          }
      }
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAttrStreamInput(nr: NodeRef.Full[T], timeRef: TimeRef, key: String,
                                       info: UGB.Input.Stream.Spec, idx: Int,
                                       bufSize: Int, value: Obj[T])
                                      (implicit tx: T): BufferAndGain = value match {
      case a: AudioCue.Obj[T] =>
        streamAudioCueToBuffer(cue = a.value, nr = nr, timeRef = timeRef, key = key, info = info,
          idx = idx, bufSize = bufSize)

      case _: Gen[T] =>
        val valueOpt: Option[Obj[T]] = for {
          observed <- genViewMap.get(key)
          tryOpt   <- observed.gen.value
          value    <- tryOpt.toOption
        } yield value

        val newValue = valueOpt.getOrElse(sys.error(s"Missing attribute $key for stream content"))
        buildAttrStreamInput(nr, timeRef, key = key, info = info, idx = idx, bufSize = bufSize, value = newValue)

      case a => sys.error(s"Cannot use attribute $a as an audio stream")
    }

    private def buildAttrStreamInputFromExpr(nr: NodeRef.Full[T], timeRef: TimeRef, key: String,
                                       info: UGB.Input.Stream.Spec, idx: Int,
                                       bufSize: Int, ex: IExpr[T, _])
                                      (implicit tx: T): BufferAndGain = ex.value match {
      case a: AudioCue =>
        streamAudioCueToBuffer(cue = a, nr = nr, timeRef = timeRef, key = key, info = info,
          idx = idx, bufSize = bufSize)

      case a => sys.error(s"Cannot use attribute $a as an audio stream")
    }

    final protected def streamAudioCueToBuffer(cue: AudioCue, nr: NodeRef.Full[T], timeRef: TimeRef, key: String,
                                               info: UGB.Input.Stream.Spec, idx: Int,
                                               bufSize: Int)(implicit tx: T): BufferAndGain = {
      val spec      = cue.spec
      val path      = cue.artifact.getAbsolutePath
      val _gain     = cue.gain
      val offsetT   = ((cue.offset + timeRef.offset) * spec.sampleRate / SampleRate + 0.5).toLong
      val _buf      = if (info.isNative) {
        // XXX DIRTY HACK
        val offset1 = if (key.contains("!rnd")) {
          val fOffset = cue.fileOffset
          fOffset + (math.random() * (spec.numFrames - fOffset)).toLong
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
          fileFrames = spec.numFrames, interpolation = info.interp, startFrame = offsetT, loop = false,
          resetFrame = offsetT)
        nr.addUser(trig)
        __buf
      }
      new BufferAndGain(_buf, _gain.toFloat)
    }

    final protected def readAudioCueToBuffer(cue: AudioCue)(implicit tx: T): Buffer = {
      val spec      = cue.spec
      val path      = cue.artifact.getAbsolutePath
      val offset    = cue.fileOffset
      val numFrames = math.max(0L, spec.numFrames - offset)
      // XXX TODO - for now, gain is ignored.
      // one might add an auxiliary control proxy e.g. Buffer(...).gain
      // val _gain     = audioElem.gain    .value
      if (cue.gain != 1.0) {
        println(s"Warning: cue gain  ${cue.gain} ignored in Buffer.read")
      }
      if (numFrames > 0x3FFFFFFF)
        sys.error(s"File too large for in-memory buffer: $path ($numFrames frames)")
      val bufSize   = spec.numFrames.toInt
      val _buf      = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
      _buf.read(path = path, fileStartFrame = offset)
      _buf
    }

    /** Sub-classes may override this if invoking the super-method.
      * If the value is incompatible with the assigned `value` and rebuilding the
      * synth-graph would alleviate that problem, a `MissingIn` should be thrown.
      * If the problem does not change in terms of the re-evaluation of the
      * synth-graph, a different generic exception must be thrown to avoid
      * an infinite loop.
      */
    protected def buildAttrInput(nr: NodeRef.Full[T], timeRef: TimeRef, key: String, value: UGB.Value)
                      (implicit tx: T): Unit = {
      value match {
        case UGB.Input.Scalar.Value(numChannels) =>  // --------------------- scalar
          auralAttrMap.get(key).foreach { a =>
            val target = AuralAttribute.Target(nr, key, Bus.audio(server, numChannels))
            a.run(timeRef = timeRef, target = target)
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
            val bufAndGain = buildAttrStreamInput(nr = nr, timeRef = timeRef, key = key,
              info = info, idx = idx, bufSize = bufSize)
            nr.addControl(ctlName -> Seq[Float](bufAndGain.buf.id.toFloat, bufAndGain.gain): ControlSet)
            val late = Buffer.disposeWithNode(bufAndGain.buf, nr)
            nr.addResource(late)
          }

        case UGB.Input.Buffer.Value(_ /* numFr */, _ /* numCh */, false) =>   // ----------------------- random access buffer
          def fromSeq(values: Vec[Float]): Buffer = {
            val bufSize   = values.size
            val _buf      = Buffer(server)(numFrames = bufSize, numChannels = 1)
            _buf.setn(values)
            _buf
          }

          // XXX TODO ugly ugly ugly
          val rb: Buffer = runnerAttr.get(key) match {
            case Some(ex: IExpr[T, _]) =>
              ex.value match {
                case a: AudioCue => readAudioCueToBuffer(a)
                case sq: Vec[_] if sq.forall(_.isInstanceOf[Double]) =>
                  val values = sq.asInstanceOf[Vec[Double]].map(_.toFloat)
                  fromSeq(values)
                case sq: Vec[_] if sq.forall(_.isInstanceOf[Int]) =>
                  val values = sq.asInstanceOf[Vec[Int]].map(_.toFloat)
                  fromSeq(values)
                case a =>
                  sys.error(s"Cannot use attribute $a as a buffer content")
              }
            case Some(a) =>
              sys.error(s"Cannot use attribute $a as a buffer content")
            case None =>
              val valueOpt = procCached().attr.get(key)
              valueOpt.fold[Buffer] {
                sys.error(s"Missing attribute $key for buffer content")
              } {
                case a: AudioCue.Obj[T] =>
                  readAudioCueToBuffer(a.value)

                case dv: DoubleVector[T] =>
                  val values = dv.value.map(_.toFloat)
                  fromSeq(values)

                case dv: IntVector[T] =>
                  val values = dv.value.map(_.toFloat)
                  fromSeq(values)

                case a => sys.error(s"Cannot use attribute $a as a buffer content")
              }
          }

          val ctlName = graph.Buffer.controlName(key)
          nr.addControl(ctlName -> rb.id)
          val late = Buffer.disposeWithNode(rb, nr)
          nr.addResource(late)

        case UGB.Input.Action.Value =>   // ----------------------- action
          val resp = new ActionResponder(objH = _objH /* tx.newHandle(nr.obj) */, key = key, synth = nr.node)
          nr.addUser(resp)

        case UGB.Input.DiskOut.Value(numCh) =>
          // XXX TODO ugly ugly ugly
          val f: File = runnerAttr.get(key) match {
            case Some(ex: IExpr[T, _]) =>
              ex.value match {
                case a: File => a
                case a =>
                  sys.error(s"Cannot use attribute $a as disk-out artifact")
              }
            case Some(a) =>
              sys.error(s"Cannot use attribute $a as disk-out artifact")
            case None =>
              val valueOpt = procCached().attr.get(key)
              valueOpt.fold[File] {
                sys.error(s"Missing attribute $key for disk-out artifact")
              } {
                case a: Artifact[T] => a.value.absolute
                case a => sys.error(s"Cannot use attribute $a as an artifact")
              }
          }
          val rb: Buffer = {
            val ext       = f.ext.toLowerCase
            val tpe       = AudioFileType.writable.find(_.extensions.contains(ext)).getOrElse(AudioFileType.AIFF)
            val _buf      = Buffer.diskOut(server)(path = f.path, fileType = tpe, numChannels = numCh)
            _buf
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

    final def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = {
      targetStateRef() = TargetPrepared
      // XXX TODO
    }

    final def run(timeRef: TimeRef.Option, unit: Unit)(implicit tx: T): Unit = {
      val tr  = timeRef.force
      val ts  = TargetPlaying(sched.time, tr)
      targetStateRef() = ts
      buildState match {
        case s: UGB.Complete[T] =>
          state match {
            case Stopped   => prepareAndLaunch(s /*, tr*/)
            case Prepared  => launch          (s, tr)
            case _ =>
          }

        case _ =>
          // state = Preparing
      }
    }

    // same as `play` but reusing previous `timeRef`
    private def playAfterRebuild()(implicit tx: T): Unit = {
      // if (state != Stopped) return
      if (state === Running) return

      (buildState, targetStateRef()) match {
        case (s: UGB.Complete[T], _ /*tp*/: TargetPlaying) =>
          prepareAndLaunch(s /*, tp.shiftTo(sched.time)*/)
        case _ =>
      }
    }

    final def stop(/* time: Long */)(implicit tx: T): Unit = {
      targetStateRef() = TargetStop
      stopForRebuild()
    }

    // same as `stop` but not touching target state
    private def stopForRebuild()(implicit tx: T): Unit = {
      freePlayingRef()
      state = Stopped
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildAsyncInput(b: AsyncProcBuilder[T], keyW: UGB.Key, value: UGB.Value)
                                 (implicit tx: T): Unit = keyW match {
      case UGB.AttributeKey(key) => buildAsyncAttrInput(b, key, value)
      case _                     => throw new IllegalStateException(s"Unsupported async input request $keyW")
    }

    /** Sub-classes may override this if falling back to the super-method. */
    protected def buildSyncInput(nr: NodeRef.Full[T], timeRef: TimeRef, keyW: UGB.Key, value: UGB.Value)
                                (implicit tx: T): Unit = keyW match {
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
//              import context.genContext

//              val au = Action.Universe[T](action, invoker = Some(invoker) /* , values = values */)
              import context.universe
              val r = Runner(action)
              r.prepare()
              r.run()
              r.dispose()
//              ctx.dispose()
            }
          }
        } (Executor.context)
        nr.addResource(late)

      case UGB.Input.StopSelf =>
        val resp = new StopSelfResponder[T](view = impl, synth = nr.node)
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
    protected def buildAsyncAttrBufferInput(b: AsyncProcBuilder[T], key: String, value: Obj[T])
                                           (implicit tx: T): Unit = value match {
      case a: AudioCue.Obj[T] =>
        buildAsyncAttrBufferInputFromAudioCue(b, key, a.value)

      case _: Gen[T] =>
        val valueOpt: Option[Obj[T]] = for {
          observed <- genViewMap.get(key)
          tryOpt   <- observed.gen.value
          value    <- tryOpt.toOption
        } yield value

        val newValue = valueOpt.getOrElse(sys.error(s"Missing attribute $key for buffer content"))
        buildAsyncAttrBufferInput(b, key, newValue)

      case a => sys.error(s"Cannot use attribute $a as a buffer content")
    }

    private def buildAsyncAttrBufferInputFromAudioCue(b: AsyncProcBuilder[T], key: String, cue: AudioCue)
                                                     (implicit tx: T): Unit = {
      val spec      = cue.spec
      val f         = cue.artifact
      val offset    = cue.fileOffset
      val numFrames = math.max(0L, spec.numFrames - offset)
      // XXX TODO - for now, gain is ignored.
      // one might add an auxiliary control proxy e.g. Buffer(...).gain
      // val _gain     = audioElem.gain    .value
      if (cue.gain != 1.0) {
        println(s"Warning: cue gain  ${cue.gain} ignored in Buffer.read")
      }
      if (numFrames > 0x3FFFFFFF)
        sys.error(s"File too large for in-memory buffer: $f ($numFrames frames)")
      val bufSize   = numFrames.toInt
      val buf       = Buffer(server)(numFrames = bufSize, numChannels = spec.numChannels)
      val cfg       = BufferPrepare.Config(f = f, spec = spec, offset = offset, buf = buf, key = key)
      b.resources ::= BufferPrepare[T](cfg)
    }

    private def buildAsyncAttrBufferInputFromExpr(b: AsyncProcBuilder[T], key: String, value: IExpr[T, _])
                                                 (implicit tx: T): Unit = value.value match {
      case a: AudioCue  => buildAsyncAttrBufferInputFromAudioCue(b, key, a)
      case a            => sys.error(s"Cannot use attribute $a as a buffer content")
    }

    /** Sub-classes may override this if invoking the super-method. */
    protected def buildAsyncAttrInput(b: AsyncProcBuilder[T], key: String, value: UGB.Value)
                                     (implicit tx: T): Unit = value match {
      case UGB.Input.Buffer.Value(_ /* numFr */, _ /* numCh */, true) =>   // ----------------------- random access buffer
        runnerAttr.get(key) match {
          case Some(a: IExpr[T, _]) => buildAsyncAttrBufferInputFromExpr(b, key, a)
          case Some(a) =>
            sys.error(s"Cannot use attribute $a as a buffer content")
          case None =>
            val bValue = b.obj.attr.get(key).getOrElse(sys.error(s"Missing attribute $key for buffer content"))
            buildAsyncAttrBufferInput(b, key, bValue)
        }

      case _ =>
        throw new IllegalStateException(s"Unsupported input attribute request $value")
    }

    // ---- asynchronous preparation ----
    private def prepareAndLaunch(ugen: UGB.Complete[T] /*, timeRef: TimeRef*/)(implicit tx: T): Unit = {
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
          import Executor.{context => ec}
          val reduced = Future.reduceLeft(res)((_, _) => ())
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

    private def prepared(ugen: UGB.Complete[T])(implicit tx: T): Unit =
      targetStateRef() match {
        case tp: TargetPlaying =>
          launch(ugen, tp.shiftTo(sched.time)) // XXX TODO - yes or no, shift time?
        case _ =>
          state = Prepared
      }

    // ---- synchronous preparation ----
    protected def launch(ugen: UGB.Complete[T], timeRef: TimeRef)(implicit tx: T): Unit = {
      val p = procCached()
      logA(s"begin launch  $p (${hashCode.toHexString})")

      val ubRes         = ugen.result
      val nameHint      = p.attr.$[StringObj](ObjKeys.attrName).map(_.value)
      val builder       = AuralNode[T](timeRef, sched.time, ubRes, server, nameHint = nameHint)
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
      state = Running
    }

    private def setPlayingPrepare(resources: List[AsyncResource[T]])(implicit tx: T): PlayingPrepare = {
      val res = new PlayingPrepare(resources)
      val old = playingRef.swap(res)
      old.dispose()
      state = Preparing
      res
    }

    private def freePlayingRef()(implicit tx: T): Unit = {
      val old = playingRef.swap(PlayingNone)
      old.dispose()
    }
  }
}