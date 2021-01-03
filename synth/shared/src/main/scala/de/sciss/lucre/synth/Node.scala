/*
 *  Node.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import de.sciss.synth.{AddAction, ControlABusMap, ControlFillRange, ControlKBusMap, ControlSet, Node => SNode}

import scala.collection.immutable.{Seq => ISeq}

trait Node extends Resource with NodeRef {
  // ---- abstract ----
  def peer: SNode

  /** Refers to itself */
  def node(implicit tx: RT): Node = this

  def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
          (implicit tx: RT): Unit

  def onEndTxn(fun: RT => Unit)(implicit tx: RT): Unit
  def onEnd   (code:   => Unit)(implicit tx: RT): Unit

  //   final def server = peer.server

  def read(assoc: (AudioBus  , String))(implicit tx: RT): AudioBusNodeSetter
  def read(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter

  /** Associates an audio bus with this node such that the node writes to this bus.
    * This creates a `DynamicAudioBusUser` which will be freed automatically when
    * this node ends.
    */
  def write    (assoc: (AudioBus  , String))(implicit tx: RT): AudioBusNodeSetter
  def write    (assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter
  def readWrite(assoc: (AudioBus  , String))(implicit tx: RT): AudioBusNodeSetter
  def readWrite(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter

  def map(assoc: (AudioBus  , String))(implicit tx: RT): AudioBusNodeSetter
  def map(assoc: (ControlBus, String))(implicit tx: RT): ControlBusNodeSetter

  def free()(implicit tx: RT): Unit

  def set (pairs: ControlSet*)(implicit tx: RT): Unit
  def setn(pairs: ControlSet*)(implicit tx: RT): Unit

  def fill(data: ControlFillRange*)(implicit tx: RT): Unit

  //   def setIfOnline( pairs: ControlSet* )( implicit tx: Txn ) : Unit

  def mapn (pairs: ControlKBusMap*)(implicit tx: RT): Unit
  def mapan(pairs: ControlABusMap*)(implicit tx: RT): Unit

  //   def moveToHeadIfOnline( group: Group )( implicit tx: Txn ) : Unit

  def moveToHead(group: Group)(implicit tx: RT): Unit
  def moveToTail(group: Group)(implicit tx: RT): Unit
  def moveBefore(target: Node)(implicit tx: RT): Unit
  def moveAfter (target: Node)(implicit tx: RT): Unit

  def run(state: Boolean)(implicit tx: RT): Unit

  /**
    * @param   releaseTime the optional release time in seconds within which the synth should fade out,
    *                      or `-1` (default) if the envelope should be released at its nominal release time.
    *                      If the `EnvGen` has a `doneAction` of `freeSelf`, the synth will be freed after
    *                      the release phase.
    */
  def release(releaseTime: Double = -1.0)(implicit tx: RT): Unit
}