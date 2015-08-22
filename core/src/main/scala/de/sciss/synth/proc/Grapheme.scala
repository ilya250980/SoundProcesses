/*
 *  Grapheme.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package synth
package proc

import java.io.File

import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.bitemp.BiPin
import de.sciss.lucre.event.Publisher
import de.sciss.lucre.expr.{Expr => _Expr}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.{event => evt, expr}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Serializer, Writable}
import de.sciss.span.Span
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.impl.{GraphemeImpl => Impl}
import de.sciss.{model => m}

import scala.annotation.switch
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

object Grapheme {
  final val typeID = 0x10003

  // If necessary for some views, we could eventually add the Elems, too,
  // like `changes: Vec[ (Elem[ S ], Value) ]`. Then the question would be
  // if Elem should have an id method? I.e. we'll have `add( elem: Elem[ S ]) : StoredElem[ S ]`
  // where `trait StoredElem[ S <: Sys[ S ]] { def elem: Elem[ S ]; def id: S#ID }`?
  final case class Update[S <: Sys[S]](grapheme: Grapheme[S], changes: Vec[Segment])

  implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Grapheme[S]] = Impl.serializer[S]

  // 0 reserved for variables
  private final val curveCookie = 1
  private final val audioCookie = 2

  object Value {
    // implicit val biType: ExprType1[Value] = Expr

    implicit object serializer extends ImmutableSerializer[Value] {
      def write(v: Value, out: DataOutput): Unit = v.write(out)

      def read(in: DataInput): Value =
        (in.readByte().toInt: @switch) match {
          case `curveCookie`  => Curve.readIdentified(in)
          case `audioCookie`  => Audio.readIdentified(in)
          case cookie         => sys.error("Unexpected cookie " + cookie)
        }
    }

    // implicit def curveFromMonoTuple(tup: (Double, synth.Curve)): Curve = Curve.fromMonoTuple(tup)

    object Curve {
      // implicit def fromMonoTuple(tup: (Double, synth.Curve)): Curve = Curve(tup)

      implicit object serializer extends ImmutableSerializer[Curve] {
        def write(v: Curve, out: DataOutput): Unit = v.write(out)
        def read(in: DataInput): Curve = {
          val cookie = in.readByte()
          require(cookie == `curveCookie`, s"Unexpected cookie $cookie")
          readIdentified(in)
        }
      }
      private[Value] def readIdentified(in: DataInput): Curve = {
        val sz      = in.readInt()
        val values  = Vec.fill(sz) {
          val mag   = in.readDouble()
          val env   = synth.Curve.serializer.read(in)
          (mag, env)
        }
        Curve(values: _*)
      }
    }

    /** A mono- or polyphonic constant value.
      *
      * @param values  pairs interpreted as target values and target shapes.
      */
    final case class Curve(values: (Double, synth.Curve)*) extends Value {
      def numChannels = values.size

      def write(out: DataOutput): Unit = {
        out.writeByte(curveCookie)
        val sz = values.size
        out.writeInt(sz)
        values.foreach { case (mag, shape) =>
          out.writeDouble(mag)
          synth.Curve.serializer.write(shape, out)
        }
      }
    }

    object Audio {
      implicit object serializer extends ImmutableSerializer[Audio] {
        def write(v: Audio, out: DataOutput): Unit = v.write(out)
        def read(in: DataInput): Audio = {
          val cookie = in.readByte()
          require(cookie == `audioCookie`, s"Unexpected cookie $cookie")
          readIdentified(in)
        }
      }
      private[Value] def readIdentified(in: DataInput): Audio = {
        val artifact  = new File(in.readUTF()) // Artifact.read(in, access)
        val spec      = AudioFileSpec.Serializer.read(in)
        val offset    = in.readLong()
        val gain      = in.readDouble()
        Audio(artifact, spec, offset, gain)
      }
    }

    /** An audio region segment.
      *
      * @param  artifact  the audio file
      * @param  spec      the audio file specification, e.g. retrieved via `AudioFile.readSpec`
      * @param  offset    the file offset in sample frames
      * @param  gain      the gain factor (linear, where 1.0 is original volume)
      */
    final case class Audio(artifact: Artifact.Value, spec: AudioFileSpec, offset: Long, gain: Double)
      extends Value {

      def numChannels = spec.numChannels

      def write(out: DataOutput): Unit = {
        out.writeByte(audioCookie)
        out.writeUTF(artifact.getPath) // artifact.write(out)
        AudioFileSpec.Serializer.write(spec, out)
        out.writeLong(offset)
        out.writeDouble(gain)
      }
    }
  }

  /** An evaluated and flattened scan element. This is either an immutable value such as a constant or
    * envelope segment, or a real-time signal, coming either from the same process (`Source`) or being
    * fed by another embedded process (`Sink`).
    */
  sealed trait Value extends Writable {
    def numChannels: Int
  }

  object Segment {
    sealed trait Defined extends Segment {
      def numChannels: Int
      final def isDefined = true
    }
    final case class Const(span: Span.HasStart, values: Vec[Double]) extends Defined {
      def numChannels = values.size
    }
    final case class Curve(span: Span, values: Vec[(Double, Double, synth.Curve)]) extends Defined {
      def numChannels = values.size
    }
    final case class Audio(span: Span.HasStart, value: Value.Audio) extends Defined {
      def numChannels = value.numChannels
    }
    final case class Undefined(span: Span.HasStart) extends Segment {
      def isDefined = false
    }
  }

  sealed trait Segment {
    def span: Span.HasStart
    def isDefined: Boolean
  }

  object Expr extends expr.impl.ExprTypeImpl[Value] {
    final val typeID = 11

    object Curve extends expr.impl.ExprTypeImpl[Value.Curve] {
      final val typeID = 12

      def apply[S <: Sys[S]](values: (_Expr[S, Double], synth.Curve)*)(implicit tx: S#Tx): Curve[S] = {
        val targets = evt.Targets[S]
        new CurveImpl(targets, values.toIndexedSeq).connect()
      }

      def unapplySeq[S <: Sys[S]](expr: Expr[S]): Option[Seq[(_Expr[S, Double], synth.Curve)]] = {
        if (expr.isInstanceOf[CurveImpl[_]]) {
          val c = expr.asInstanceOf[CurveImpl[S]]
          Some(c.values)
        } else {
          None
        }
      }

      def valueSerializer: ImmutableSerializer[Value.Curve] = Value.Curve.serializer

//      def readValue(in: DataInput): Value.Curve = Value.Curve.serializer.read(in)
//      def writeValue(v: Value.Curve, out: DataOutput): Unit = v.write(out)

//      override protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc,
//                                               targets: evt.Targets[S])(implicit tx: S#Tx): Curve[S] with evt.Node[S] = {
//        if (cookie != curveCookie) sys.error(s"Unexpected cookie $cookie")
//        readIdentifiedTuple(in, access, targets)
//      }

      private[Grapheme] def readIdentifiedTuple[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                            (implicit tx: S#Tx): Curve[S] with evt.Node[S] = {
        val sz      = in.readInt()
        val values  = Vector.fill(sz) {
          val mag   = lucre.expr.Double.read(in, access)
          val shape = synth.Curve.serializer.read(in)
          (mag, shape)
        }
        new CurveImpl(targets, values)
      }
    }
    sealed trait Curve[S <: Sys[S]] extends Expr[S] with _Expr[S, Value.Curve]

    object Audio extends expr.impl.ExprTypeImpl[Value.Audio] {
      final val typeID = 13

      def apply[S <: Sys[S]](artifact: Artifact[S], spec: AudioFileSpec, offset: _Expr[S, Long], gain: _Expr[S, Double])
                                (implicit tx: S#Tx): Audio[S] = {
        val targets = evt.Targets[S]
        new AudioImpl(targets, artifact, spec, offset, gain).connect()
      }

      def unapply[S <: Sys[S]](expr: Expr[S]): Option[(Artifact[S], AudioFileSpec, _Expr[S, Long], _Expr[S, Double])] = {
        if (expr.isInstanceOf[AudioImpl[_]]) {
          val a = expr.asInstanceOf[AudioImpl[S]]
          Some((a.artifact, a.spec, a.offset, a.gain))
        } else {
          None
        }
      }

      def valueSerializer: ImmutableSerializer[Value.Audio] = Value.Audio.serializer

//      def readValue(in: DataInput): Value.Audio = Value.Audio.serializer.read(in)
//      def writeValue(v: Value.Audio, out: DataOutput): Unit = v.write(out)

//      override protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc,
//                                           targets: evt.Targets[S])(implicit tx: S#Tx): Audio[S] with evt.Node[S] = {
//        require(cookie == audioCookie, s"Unexpected cookie $cookie")
//        readIdentifiedTuple(in, access, targets)
//      }

      private[Grapheme] def readIdentifiedTuple[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                             (implicit tx: S#Tx): Audio[S] with evt.Node[S] = {
        val artifact  = Artifact.read(in, access)
        val spec      = AudioFileSpec.Serializer.read(in)
        val offset    = lucre.expr.Long  .read(in, access)
        val gain      = lucre.expr.Double.read(in, access)
        new AudioImpl(targets, artifact, spec, offset, gain)
      }
    }
    sealed trait Audio[S <: Sys[S]] extends Expr[S] with _Expr[S, Value.Audio] {
      def artifact: Artifact[S]
      def offset  : _Expr[S, Long  ]
      def gain    : _Expr[S, Double]
      def spec    : AudioFileSpec
    }

    private final class CurveImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                   val values: Vec[(_Expr[S, Double], synth.Curve)])
      extends expr.impl.NodeImpl[S, Value.Curve] with Curve[S] {

      def typeID: Int = Curve.typeID

      def value(implicit tx: S#Tx): Value.Curve = {
        val v = values.map {
          case (mag, shape) => mag.value -> shape
        }
        Value.Curve(v: _*)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        values.foreach { tup =>
          tup._1.changed ---> changed
        }
        this
      }

      private[this] def disconnect()(implicit tx: S#Tx): Unit =
        values.foreach { tup =>
          tup._1.changed -/-> changed
        }

      object changed extends Changed {
        def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[m.Change[Value.Curve]] = {
          val beforeVals  = Vec.newBuilder[(Double, synth.Curve)]
          val nowVals     = Vec.newBuilder[(Double, synth.Curve)]
          values.foreach {
            case (mag, shape) =>
              val magEvt = mag.changed
              if (pull.contains(magEvt)) {
                pull(magEvt) match {
                  case Some(m.Change(magBefore, magNow)) =>
                    beforeVals += magBefore -> shape
                    nowVals    += magNow    -> shape
                  case _ =>
                    val mv      = mag.value
                    beforeVals += mv -> shape
                    nowVals    += mv -> shape
                }
              } else {
                val mv      = mag.value
                beforeVals += mv -> shape
                nowVals    += mv -> shape
              }
          }
          val before  = Value.Curve(beforeVals.result(): _*)
          val now     = Value.Curve(nowVals.result(): _*)
          if (before == now) None else Some(model.Change(before, now))
        }
      }

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(curveCookie)
        val sz = values.size
        out.writeInt(sz)
        values.foreach { tup =>
          tup._1.write(out)
          synth.Curve.serializer.write(tup._2, out)
        }
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      override def toString: String = s"Elem.Curve$id"
    }

    private final class AudioImpl[S <: Sys[S]](protected val targets: evt.Targets[S], val artifact: Artifact[S],
                                                   val spec: AudioFileSpec, val offset: _Expr[S, Long],
                                                   val gain: _Expr[S, Double])
      extends expr.impl.NodeImpl[S, Value.Audio] with Audio[S] {

      def typeID: Int = Audio.typeID

      def value(implicit tx: S#Tx): Value.Audio = {
        val artVal    = artifact.value
        val offsetVal = offset  .value
        val gainVal   = gain    .value
        Value.Audio(artVal, spec, offsetVal, gainVal)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        artifact.changed ---> changed
        offset  .changed ---> changed
        gain    .changed ---> changed
        this
      }

      private[this] def disconnect()(implicit tx: S#Tx): Unit = {
        artifact.changed -/-> changed
        offset  .changed -/-> changed
        gain    .changed -/-> changed
      }

      object changed extends Changed {
        def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[m.Change[Value.Audio]] = {
          val artEvt = artifact.changed
          val artOpt = if (pull.contains(artEvt)) pull(artEvt) else None
          val (artBefore, artNow) = artOpt.fold({
            val art = artifact.value
            (art, art)
          })(_.toTuple)

          val offsetEvt = offset.changed
          val offsetOpt = if (pull.contains(offsetEvt)) pull(offsetEvt) else None
          val (offsetBefore, offsetNow) = offsetOpt.fold({
            val ov = offset.value
            (ov, ov)
          })(_.toTuple)

          val gainEvt = gain.changed
          val gainOpt = if (pull.contains(gainEvt)) pull(gainEvt) else None
          val (gainBefore, gainNow) = gainOpt.fold({
            val gv = gain.value
            (gv, gv)
          })(_.toTuple)

          val before  = Value.Audio(artBefore, spec, offsetBefore, gainBefore)
          val now     = Value.Audio(artNow   , spec, offsetNow   , gainNow   )
          if (before == now) None else Some(model.Change(before, now))
        }
      }

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(audioCookie)
        artifact.write(out)
        AudioFileSpec.Serializer.write(spec, out)
        offset.write(out)
        gain.write(out)
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      override def toString: String = s"Elem.Audio$id"
    }

    // ---- bi-type ----

    //    def longType    : BiType[Long    ] = Longs
    //    def spanLikeType: BiType[SpanLike] = SpanLikes

    def valueSerializer: ImmutableSerializer[Value] = Value.serializer

//    override protected def readNode[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: evt.Targets[S])
//                                            (implicit tx: S#Tx): Expr[S] with evt.Node[S] = {
//      (cookie: @switch) match {
//        case `curveCookie` => Curve.readIdentifiedTuple(in, access, targets)
//        case `audioCookie` => Audio.readIdentifiedTuple(in, access, targets)
//        case _ => sys.error(s"Unexpected cookie $cookie")
//      }
//    }
  }

  //  type Elem[S <: Sys[S]] = Expr[S, Value]
  sealed trait Expr[S <: Sys[S]] extends _Expr[S, Value]

  type TimedElem[S <: Sys[S]] = BiPin.Entry[S, Expr[S]]

  trait Modifiable[S <: Sys[S]] extends Grapheme[S] {
    def add   (elem: TimedElem[S])(implicit tx: S#Tx): Unit
    def remove(elem: TimedElem[S])(implicit tx: S#Tx): Boolean
    def clear()(implicit tx: S#Tx): Unit
  }

  def apply[S <: Sys[S]](numChannels: Int)(implicit tx: S#Tx): Modifiable[S] = Impl.modifiable[S](numChannels)

  object Modifiable {
    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Modifiable[S] =
      Impl.readModifiable(in, access)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Modifiable[S]] =
      Impl.modifiableSerializer[S]

    /** Extractor to check if a `Grapheme` is actually a `Grapheme.Modifiable`. */
    def unapply[S <: Sys[S]](g: Grapheme[S]): Option[Modifiable[S]] = {
      if (g.isInstanceOf[Modifiable[_]]) Some(g.asInstanceOf[Modifiable[S]]) else None
    }
  }

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Grapheme[S] = Impl.read(in, access)
}
trait Grapheme[S <: Sys[S]] extends evt.Node[S] with Publisher[S, Grapheme.Update[S]] {
  import Grapheme.{Modifiable, Segment, TimedElem, Value}

  /** The idea of all traits which distinguish between read-only and modifiable sub-type is that
    * eventually the super-type acts somewhat like an expression. It might be possible to map
    * a grapheme with operators, and it might be preferable to avoid having to have the modifiable
    * operations in the mapped object (cf. `Expr` versus `Expr.Var`).
    */
  def modifiableOption: Option[Modifiable[S]]

  def at     (time: Long)(implicit tx: S#Tx): Option[TimedElem[S]]
  def valueAt(time: Long)(implicit tx: S#Tx): Option[Value]
  def segment(time: Long)(implicit tx: S#Tx): Option[Segment.Defined]

  def eventAfter(time: Long)(implicit tx: S#Tx): Option[Long]
  def firstEvent(implicit tx: S#Tx): Option[Long]

  def numChannels: Int

  def debugList()(implicit tx: S#Tx): List[Segment.Defined]
}