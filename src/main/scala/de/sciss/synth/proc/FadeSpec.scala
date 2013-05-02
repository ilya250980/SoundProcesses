package de.sciss
package synth
package proc

import lucre.{event => evt, bitemp}
import lucre.expr.Expr
import serial.{DataOutput, DataInput, ImmutableSerializer}
import bitemp.BiType
import synth.proc.impl.CommonSerializers
import de.sciss.lucre.event.{Change, Pull, Targets}
import synth.expr.{SpanLikes, Longs}
import span.SpanLike

object FadeSpec {
  private final val COOKIE = 0x4664 // 'Fd'

  object Value {
    implicit object serializer extends ImmutableSerializer[Value] {
      def write(v: Value, out: DataOutput) {
        import v._
        out.writeShort(COOKIE)
        out.writeLong (numFrames)
        CommonSerializers.EnvConstShape.write(shape, out)
        out.writeFloat(floor)
      }

      def read(in: DataInput): Value = {
        val cookie = in.readShort()
        require(cookie == COOKIE, s"Unexpected cookie $cookie, expected $COOKIE")
        val numFrames = in.readLong()
        val shape     = CommonSerializers.EnvConstShape.read(in)
        val floor = in.readFloat()
        Value(numFrames = numFrames, shape = shape, floor = floor)
      }
    }
  }
  final case class Value(numFrames: Long, shape: Env.ConstShape = linShape, floor: Float = 0f)

  object Elem extends BiType[Value] {
    final val typeID = 14

    def longType    : BiType[Long    ] = Longs
    def spanLikeType: BiType[SpanLike] = SpanLikes

    def readValue(in: DataInput): Value = Value.serializer.read(in)

    def writeValue(value: Value, out: DataOutput) {
      Value.serializer.write(value, out)
    }

    def apply[S <: evt.Sys[S]](numFrames: Expr[S, Long], shape: Expr[S, Env.ConstShape], floor: Expr[S, Double])
                              (implicit tx: S#Tx): Elem[S] = {
      val targets = Targets.partial[S] // XXX TODO partial?
      new Impl(targets, numFrames, shape, floor)
    }

    def unapply[S <: evt.Sys[S]](expr: Elem[S]): Option[(Expr[S, Long], Expr[S, Env.ConstShape], Expr[S, Double])] =
      expr match {
        case impl: Impl[S] => Some((impl.numFrames, impl.shape, impl.floor))
        case _ => None
      }

    protected def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                              (implicit tx: S#Tx): ReprNode[S] = {
      ??? // XXX TODO: should we care about readTuple?
    }

    private final class Impl[S <: evt.Sys[S]](protected val targets: Targets[S],
                                              val numFrames: Expr[S, Long],
                                              val shape: Expr[S, Env.ConstShape],
                                              val floor: Expr[S, Double])
      extends lucre.expr.impl.NodeImpl[S, Value] with Elem[S] {

      def value(implicit tx: S#Tx) = Value(numFrames.value, shape.value, floor.value.toFloat)

      protected def reader = Elem.serializer[S]

      def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[evt.Change[Value]] = {
        val framesEvt = numFrames.changed
        val framesChO = if (framesEvt.isSource(pull)) framesEvt.pullUpdate(pull) else None
        val shapeEvt  = shape.changed
        val shapeChO  = if (shapeEvt .isSource(pull)) shapeEvt .pullUpdate(pull) else None
        val floorEvt  = floor.changed
        val floorChO  = if (floorEvt .isSource(pull)) floorEvt .pullUpdate(pull) else None

        if (framesChO.isEmpty && shapeChO.isEmpty && floorChO.isEmpty) return None

        val framesCh = framesChO.getOrElse {
          val framesV = numFrames.value
          Change(framesV, framesV)
        }

        val shapeCh = shapeChO.getOrElse {
          val shapeV = shape.value
          Change(shapeV, shapeV)
        }

        val floorCh = floorChO.getOrElse {
          val floorV = floor.value
          Change(floorV, floorV)
        }

        val before  = Value(framesCh.before, shapeCh.before, floorCh.before.toFloat)
        val now     = Value(framesCh.now,    shapeCh.now,    floorCh.now   .toFloat)

        Some(Change(before, now))
      }

      protected def writeData(out: DataOutput) {
        numFrames.write(out)
        shape    .write(out)
        floor    .write(out)
      }

      def connect()(implicit tx: S#Tx) {
        numFrames.changed ---> this
        shape    .changed ---> this
        floor    .changed ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        numFrames.changed -/-> this
        shape    .changed -/-> this
        floor    .changed -/-> this
      }
    }
  }
  sealed trait Elem[S <: evt.Sys[S]] extends Expr[S, FadeSpec.Value]
}