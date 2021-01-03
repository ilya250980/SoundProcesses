/*
 *  BufferOut.scala
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

package de.sciss.synth.proc.graph

import de.sciss.proc.UGenGraphBuilder
import de.sciss.proc.UGenGraphBuilder.{Input, MissingIn}
import de.sciss.synth.ugen.ControlProxy
import de.sciss.synth.{GE, Rate, UGenIn, UGenInLike, control, scalar, ugen}

object BufferOut {
  def ir(artifact: String, action: String, numFrames: GE, numChannels: GE = 1): BufferOut =
    new BufferOut(scalar , artifact, numFrames = numFrames, numChannels = numChannels, action = action)
  
  def kr(artifact: String, action: String, numFrames: GE, numChannels: GE = 1): BufferOut =
    new BufferOut(control, artifact, numFrames = numFrames, numChannels = numChannels, action = action)

  /** Convenience alias for `kr` */
  def apply(artifact: String, action: String, numFrames: GE, numChannels: GE = 1): BufferOut =
    kr(artifact, numFrames = numFrames, numChannels = numChannels, action = action)

  /* private[proc] */ def controlName(artifact: String, action: String): String = s"$$buf_${artifact}_$action"

  private[graph] def canResolve(in: GE): Either[String, Unit] = {
    import ugen._
    in match {
      case _: Constant            => Right(())
      case _: Attribute           => Right(())
      case UnaryOpUGen (_, a   )  => canResolve(a)
      case BinaryOpUGen(_, a, b)  =>
        for {
          _ <- canResolve(a).right
          _ <- canResolve(b).right
        } yield ()

      case _: SampleRate          => Right(())
      case _: NumChannels         => Right(())
      case _                      => Left(s"Element: $in")
    }
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

  private[graph] def resolveFloat(in: GE, builder: UGenGraphBuilder): Either[String, Float] = {
    import ugen._
    in match {
      case Constant(f) => Right(f)

      case a: Attribute =>
        val input = UGenGraphBuilder.Input.Attribute(a.key)
        val opt   = builder.requestInput(input)  // .asInstanceOf[UGenGraphBuilder.Input.Attribute.Value]
        opt.peer.fold[Either[String, Float]] {
          a.default.fold[Either[String, Float]] {
            throw MissingIn(input.key)
          } { sq =>
            if (sq.size == 1) Right(sq.head)
            else Left(s"Cannot use multi-channel element as single Float: $sq")
          }
        } {
          case i: Int     => Right(i.toFloat)
          case d: Double  => Right(d.toFloat)
          case n: Long    => Right(n.toFloat)
          case b: Boolean => Right(if (b) 1f else 0f)
          case other      => Left(s"Cannot convert attribute value to Float: $other")
        }

      case UnaryOpUGen(op: UnaryOpUGen.PureOp, a)  =>
        val af = resolveFloat(a, builder)
        af.right.map(op.make1)

      case BinaryOpUGen(op: BinaryOpUGen.PureOp, a, b) =>
        for {
          af <- resolveFloat(a, builder).right
          bf <- resolveFloat(b, builder).right
        } yield op.make1(af, bf)

      case _: SampleRate   =>
        val sr = builder.server.sampleRate.toFloat
        Right(sr)

      case NumChannels(in0) =>
        var uIns    = Vector.empty[UGenIn]
        var uInsOk  = true
        var exp     = 0
        val args    = in0.expand.outputs
        args.foreach(_.unbubble match {
          case u: UGenIn => if (uInsOk) uIns :+= u
          case g: ugen.UGenInGroup =>
            exp     = math.max(exp, g.numOutputs)
            uInsOk  = false // don't bother adding further UGenIns to uIns
        })
        if (uInsOk) {
          Right(uIns.size.toFloat)
        } else {
          Left(s"Cannot use multi-channel element as single Float: $in0")
        }

//      case g: ugen.UGenInGroup =>
//        if (g.numOutputs == 1) resolveFloat(g.outputs.head, builder)
//        else Left(s"Cannot convert multi-channel element to Float: $in")
    }
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

  private def fail(arg: String, detail: String): Nothing =
    throw new IllegalArgumentException(s"BufferOut.$arg cannot be resolved at initialization time: $detail")

  canResolve(numFrames  ).left.foreach(fail("numFrames"  , _))
  canResolve(numChannels).left.foreach(fail("numChannels", _))

  protected def makeUGens: UGenInLike = {
    val b             = UGenGraphBuilder.get
    val numFramesI    = resolveFloat(numFrames  , b).fold[Float](fail("numFrames"  , _), identity).toInt
    val numChannelsI  = resolveFloat(numChannels, b).fold[Float](fail("numChannels", _), identity).toInt

    b.requestInput(Input.BufferOut(artifact = artifact, action = action,
      numFrames = numFramesI, numChannels = numChannelsI))
    val ctlName       = BufferOut.controlName(artifact = artifact, action = action)
    val ctl           = ControlProxy(rate, Vector(0f), Some(ctlName))
    ctl.expand
  }
}