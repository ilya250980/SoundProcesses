/*
 *  BufferOut.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package graph

import de.sciss.synth.proc.UGenGraphBuilder.Input
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, ControlProxy, UnaryOpUGen}

object BufferOut {
  def ir(artifact: String, action: String, numFrames: GE, numChannels: GE = 1): BufferOut =
    new BufferOut(scalar , artifact, numFrames = numFrames, numChannels = numChannels, action = action)
  
  def kr(artifact: String, action: String, numFrames: GE, numChannels: GE = 1): BufferOut =
    new BufferOut(control, artifact, numFrames = numFrames, numChannels = numChannels, action = action)

  /** Convenience alias for `kr` */
  def apply(artifact: String, action: String, numFrames: GE, numChannels: GE = 1): BufferOut =
    kr(artifact, numFrames = numFrames, numChannels = numChannels, action = action)

  /* private[proc] */ def controlName(artifact: String, action: String): String = s"$$buf_${artifact}_$action"

  private def canResolve(in: GE): Boolean = in match {
    case _: Constant            => true
    case _: Attribute           => true
    case UnaryOpUGen (_, a   )  => canResolve(a)
    case BinaryOpUGen(_, a, b)  => canResolve(a) && canResolve(b)
  }

//  private def resolveInt(in: GE, builder: UGenGraphBuilder): Int = in match {
//    case Constant(f) => f.toInt
//    case a: Attribute => ...
//    case UnaryOpUGen(op, a) =>
//      // support a few ops directly without having to go back to float conversion
//      op match {
//        case UnaryOpUGen.Neg  => -resolveInt(a, builder)
//        case UnaryOpUGen.Abs  => math.abs(resolveInt(a, builder))
//        case _                => resolveFloat(in, builder).toInt
//      }
//    case BinaryOpUGen(op, a, b) =>
//      // support a few ops directly without having to go back to float conversion
//      op match {
//        case BinaryOpUGen.Plus  => ...
//        case _                  => resolveFloat(in, builder).toInt
//      }
//      resolveFloat(in, builder).toInt
//  }

  private def resolveFloat(in: GE, builder: UGenGraphBuilder): Float = in match {
    case Constant(f) => f
    case a: Attribute => ???
    case UnaryOpUGen(op, a)  =>
      val af = resolveFloat(a, builder)
      op.make1(af)
    case BinaryOpUGen(op, a, b) =>
      val af = resolveFloat(a, builder)
      val bf = resolveFloat(a, builder)
      op.make1(af, bf)
  }
}
/** A graph element that creates an empty buffer for the synth graph to write to. Upon completion
  * of the encompassing proc, the buffer contents is written to an artifact referred to by its
  * attribute-map key. When the file has been written, the action referred to by its attribute-map
  * key is called. The element outputs the buffer-id.
  *
  * @param rate         the rate at which the buffer-id is presented
  * @param artifact     a key into the encompassing object's attribute map, leading to an `Artifact`.
  * @param action       a key into the encompassing object's attribute map, leading to an `Action`.
  * @param numFrames    the number of frames to allocate
  * @param numChannels  the number of channels to allocate
  */
final case class BufferOut(rate: Rate, artifact: String, action: String, numFrames: GE, numChannels: GE)
  extends GE.Lazy {

  import BufferOut.{canResolve, resolveFloat}

  require(canResolve(numFrames  ), "BufferOut.numFrames cannot be resolved at initialization time")
  require(canResolve(numChannels), "BufferOut.numChannels cannot be resolved at initialization time")

  protected def makeUGens: UGenInLike = {
    val b             = UGenGraphBuilder.get
    val numFramesI    = resolveFloat(numFrames  , b).toInt
    val numChannelsI  = resolveFloat(numChannels, b).toInt

    b.requestInput(Input.BufferOut(artifact = artifact, action = action,
      numFrames = numFramesI, numChannels = numChannelsI))
    val ctlName       = BufferOut.controlName(artifact = artifact, action = action)
    val ctl           = ControlProxy(rate, Vector(0f), Some(ctlName))
    ctl.expand
  }
}