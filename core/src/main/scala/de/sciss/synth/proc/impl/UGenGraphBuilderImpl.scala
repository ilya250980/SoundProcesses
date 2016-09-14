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
import de.sciss.synth.NestedUGenGraphBuilder.ExpIfCase
import de.sciss.synth.proc.{UGenGraphBuilder => UGB}
import de.sciss.synth.{NestedUGenGraphBuilder, SynthGraph}

object UGenGraphBuilderImpl {
  import UGenGraphBuilder.{Complete, Context, Incomplete, MissingIn, State}

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
    val in  = new IncompleteImpl[S](g, Set.empty)
    in
  }

  // ---- impl ----

  private final class IncompleteImpl[S <: Sys[S]](g: SynthGraph, val rejectedInputs: Set[UGenGraphBuilder.Key])
    extends Incomplete[S] {

    override def toString = s"UGenGraphBuilder.Incomplete@${hashCode.toHexString}"

    def acceptedInputs  = Map.empty[UGB.Key, (UGB.Input, UGB.Input#Value)]
    def outputs         = Map.empty[String, Int]

    def retry(context: Context[S])(implicit tx: S#Tx): State[S] =
      new OuterImpl[S](context, this, tx).tryBuild(g)
  }

  private final class CompleteImpl[S <: Sys[S]](val result: NestedUGenGraphBuilder.Result,
      val outputs       : Map[String, Int],
      val acceptedInputs: Map[UGenGraphBuilder.Key, (UGB.Input, UGB.Input#Value)]
   ) extends Complete[S] {

    override def toString = s"UGenGraphBuilder.Complete@${hashCode.toHexString}"
  }

  private final class OuterImpl[S <: Sys[S]](protected val context: Context[S],
                                             protected val in: IncompleteImpl[S], protected val tx: S#Tx)
    extends NestedUGenGraphBuilder.Outer with Impl[S] {

  }

  private trait Impl[S <: Sys[S]]
    extends NestedUGenGraphBuilder.Basic with UGB with UGB.IO[S] {
    builder =>

    // ---- abstract ----

    protected def in: IncompleteImpl[S]
    protected def context: Context[S]
    protected def tx: S#Tx

    // ---- impl ----

    override def toString = s"UGenGraphBuilder.Incomplete@${hashCode.toHexString} (active)"

    protected def mkInner(childId: Int, thisExpIfCase: Option[ExpIfCase], parent: NestedUGenGraphBuilder.Basic,
                          name: String): NestedUGenGraphBuilder.Inner = ??? // NNN

    var outputs         = in.outputs
    var acceptedInputs  = in.acceptedInputs

    def server: Server = context.server

    def retry(context: Context[S])(implicit tx: S#Tx): State[S] =
      throw new IllegalStateException("Cannot retry an ongoing build")

    def requestInput(req: UGB.Input): req.Value = {
      // we pass in `this` and not `in`, because that way the context
      // can find accepted inputs that have been added during the current build cycle!
      val res = context.requestInput[req.Value](req, this)(tx)
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

    def tryBuild(g: SynthGraph): State[S] =
      try {
        val result = build(g)
        new CompleteImpl[S](result, outputs = outputs, acceptedInputs = acceptedInputs)
      } catch {
        case MissingIn(rejected) =>
          new IncompleteImpl[S](g, Set(rejected))
      }
  }
}