/*
 *  Synth.scala
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

import de.sciss.lucre.synth.impl.{SynthImpl => Impl}
import de.sciss.synth.{AddAction, ControlSet, SynthGraph, UGenGraph, addToHead, Synth => SSynth, SynthDef => SSynthDef}

import scala.collection.immutable.{Seq => ISeq}

object Synth {
  def apply(server: Server, graph: SynthGraph, nameHint: Option[String] = None)(implicit tx: RT): Synth = {
    val ugenGraph = graph.expand(de.sciss.synth.impl.DefaultUGenGraphBuilderFactory)
    val df        = server.acquireSynthDef(ugenGraph, nameHint)
    val res       = create(df)
    // releaseDefOnEnd(res)
    res
  }

//  private def releaseDefOnEnd(x: Synth)(implicit tx: RT): Unit =
//    x.onEndTxn { implicit tx =>
//      NodeGraph.releaseSynthDef(x.definition)
//    }

  def play(graph: SynthGraph, nameHint: Option[String] = None)
          (target: Node, args: ISeq[ControlSet] = Nil, addAction: AddAction = addToHead,
            dependencies: List[Resource] = Nil)(implicit tx: RT): Synth = {
    val res = apply(target.server, graph, nameHint)
    res.play(target, args, addAction, dependencies)
    res
  }

  /** Like `play` but does not memoize synth def. */
  def playOnce(graph: SynthGraph, nameHint: Option[String] = None)
          (target: Node, args: ISeq[ControlSet] = Nil, addAction: AddAction = addToHead,
           dependencies: List[Resource] = Nil)(implicit tx: RT): Synth = {

    // XXX TODO - DRY - NodeGraphImpl
    val server  = target.server
    val name    = server.mkSynthDefName(nameHint)
    val uGraph  = graph.expand(de.sciss.synth.impl.DefaultUGenGraphBuilderFactory)
    val peer    = SSynthDef(name, uGraph)
    val rd      = impl.SynthDefImpl(server, peer) // (bytes)
    rd.recv()

    val res = create(rd)
    res.play(target, args, addAction, dependencies)

    rd.dispose()  // free it immediately

    res
  }

  def expanded(server: Server, graph: UGenGraph, nameHint: Option[String] = None)(implicit tx: RT): Synth = {
    val df = server.acquireSynthDef(graph, nameHint)
    val res = create(df)
    res
  }

  private def create(df: SynthDef)(implicit tx: RT): Synth = {
    val server  = df.server
    val nodeId  = server.nextNodeId()
    Impl(SSynth(server.peer, nodeId), df)
  }
}

trait Synth extends Node {
  def peer: SSynth

  def definition: SynthDef
}