/*
 *  UGenGraphBuilderImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.synth.impl.BasicUGenGraphBuilder
import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.ControlProxyLike
import de.sciss.synth.{Lazy, SynthGraph, UGen, UGenGraph}

import scala.collection.immutable.{IndexedSeq => Vec}

object UGenGraphBuilderImpl {
  import UGenGraphBuilder.{State, Complete, Incomplete, MissingIn, Context}

  /** '''Note''': The resulting object is mutable, therefore must not be shared across threads and also must be
    * created and consumed within the same transaction. That is to say, to be transactionally safe, it may only
    * be stored in a `TxnLocal`, but not a full STM ref.
    */
  def apply[S <: Sys[S]](context: Context[S], proc: Proc[S])
                        (implicit tx: S#Tx): State[S] = {
    val in = init(proc)
    in.retry(context)
  }

  def init[S <: Sys[S]](proc: Proc[S])
                       (implicit tx: S#Tx): Incomplete[S] = {
    val g   = proc.graph.value
    val in  = new IncompleteImpl[S](
      remaining = g.sources, controlProxies = g.controlProxies,
      ugens = Vec.empty, controlValues = Vec.empty, controlNames = Vec.empty,
      sourceMap = Map.empty, outputs = Map.empty, acceptedInputs = Map.empty,
      rejectedInputs = Set.empty
    )
    in
  }

  // ---- impl ----

  private final class IncompleteImpl[S <: Sys[S]](
      val remaining     : Vec[Lazy],
      val controlProxies: Set[ControlProxyLike],
      val ugens         : Vec[UGen],
      val controlValues : Vec[Float],
      val controlNames  : Vec[(String, Int)],
      val sourceMap     : Map[AnyRef, Any],
      val outputs       : Map[String, Int],
      val acceptedInputs: Map[UGenGraphBuilder.Key, (Input, Input#Value)],
      val rejectedInputs: Set[UGenGraphBuilder.Key]
   )
    extends Incomplete[S] {

    override def toString = s"UGenGraphBuilder.Incomplete@${hashCode.toHexString}"

    def retry(context: Context[S])(implicit tx: S#Tx): State[S] =
      new Impl[S](context, this, tx).tryBuild()
  }

  private final class CompleteImpl[S <: Sys[S]](val result: UGenGraph,
      val outputs       : Map[String, Int],
      val acceptedInputs: Map[UGenGraphBuilder.Key, (Input, Input#Value)]
   )
    extends Complete[S] {

    override def toString = s"UGenGraphBuilder.Complete@${hashCode.toHexString}"
  }

  private final class Impl[S <: Sys[S]](context: Context[S], in: IncompleteImpl[S], val tx: S#Tx)
    extends BasicUGenGraphBuilder with UGenGraphBuilder with Incomplete[S] {
    builder =>

    override def toString = s"UGenGraphBuilder.Incomplete@${hashCode.toHexString} (active)"

    private[this] var remaining       = in.remaining
    private[this] var controlProxies  = in.controlProxies

    var outputs                 = in.outputs
    var acceptedInputs          = in.acceptedInputs
    var rejectedInputs          = Set.empty[UGenGraphBuilder.Key]

    def server: Server = context.server

    def retry(context: Context[S])(implicit tx: S#Tx): State[S] =
      throw new IllegalStateException("Cannot retry an ongoing build")

    // def sensorBus: SControlBus = aural.sensorBus

    //    def addScanIn(key: String, numChannels: Int): Int = {
    //      val fixed = numChannels >= 0
    //      val res   = aural.scanInNumChannels(key = key, numChannels = numChannels)(tx)
    //      scanIns  += key -> ScanIn(numChannels = res, fixed = fixed)
    //      res
    //    }

    def requestInput(req: Input): req.Value = {
      // we pass in `this` and not `in`, because that way the context
      // can find accepted inputs that have been added during the current build cycle!
      val res = context.requestInput[req.Value](req, this /* in */)(tx)
      acceptedInputs += req.key -> (req, res)
      logAural(s"acceptedInputs += ${req.key} -> $res")
      res
    }

    def addOutput(key: String, numChannels: Int): Unit =
      outputs.get(key).fold {
        outputs += key -> numChannels
      } { prevChans =>
        if (numChannels != prevChans) {
          val s1 = s"Cannot write multiple times to the same scan ($key)"
          val s2 = s"using different number of channels ($prevChans, $numChannels)"
          sys.error(s"$s1 $s2")
        }
      }

    //    def addAttributeIn(key: String): Int = {
    //      val res       = aural.attrNumChannels(key = key)(tx)
    //      attributeIns += key
    //      res
    //    }

    //    def addStreamIn(key: String, info: StreamIn): (Int, Int) = {
    //      val numCh = aural.attrNumChannels(key = key)(tx)
    //      val idx   = if (info.isEmpty) {
    //        if (!streamIns.contains(key)) streamIns += key -> Nil
    //        0
    //      } else {
    //        val oldValue = streamIns.getOrElse(key, Nil)
    //        streamIns += key -> (info :: oldValue)
    //        oldValue.size
    //      }
    //      (numCh, idx)
    //    }

    def tryBuild(): State[S] = UGenGraph.use(this) {
      var missingElems  = Vector.empty[Lazy]
      // var someSucceeded = false
      while (remaining.nonEmpty) {  // XXX TODO: this can go through many exceptions. perhaps should short circuit?
        val g = SynthGraph {
          remaining.foreach { elem =>
            // save rollback information -- not very elegant; should figure out how scala-stm nesting works
            val savedSourceMap      = sourceMap
            val savedControlNames   = controlNames
            val savedControlValues  = controlValues
            val savedUGens          = ugens
            val savedScanOuts       = outputs
            val savedAcceptedInputs = acceptedInputs
            try {
              elem.force(builder)
              // someSucceeded = true
            } catch {
              case MissingIn(rejected) =>
                sourceMap           = savedSourceMap
                controlNames        = savedControlNames
                controlValues       = savedControlValues
                ugens               = savedUGens
                outputs            = savedScanOuts
                acceptedInputs      = savedAcceptedInputs
                missingElems      :+= elem
                rejectedInputs     += rejected // .key
                // logAural(s"rejectedInputs += ${rejected /* .key */}")
            }
          }
        }
        if (g.nonEmpty) {
          remaining        = g.sources
          controlProxies ++= g.controlProxies
        } else {
          remaining        = Vector.empty
        }
      }

      val newState = if (missingElems.isEmpty) {
        val result = build(controlProxies)
        new CompleteImpl[S](result, outputs = outputs, acceptedInputs = acceptedInputs)

      } else {
        // NOTE: we have to return a new object even if no elem succeeded,
        // because `rejectedInputs` may contain a new key!

        // if (someSucceeded) {
          new IncompleteImpl[S](
            remaining = missingElems, controlProxies = controlProxies,
            ugens = ugens, controlValues = controlValues, controlNames = controlNames,
            sourceMap = sourceMap, outputs = outputs, acceptedInputs = acceptedInputs,
            rejectedInputs = rejectedInputs
          )
        // } else in
      }

      newState
    }
  }
}