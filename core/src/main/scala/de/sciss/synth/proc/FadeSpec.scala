/*
 *  FadeSpec.scala
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

import de.sciss.lucre.event.{Pull, Targets}
import de.sciss.lucre.expr.{DoubleObj, LongObj, Type, Expr => _Expr}
import de.sciss.lucre.stm.{Copy, Elem, Sys}
import de.sciss.lucre.{expr, stm}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.Curve
import de.sciss.synth.Curve.linear
import de.sciss.{lucre, model => m}

object FadeSpec {
  final val typeID = 14

  /* override */ def init(): Unit = {
    // super.init()
    Obj  .init()
  }

  private final val COOKIE = 0x4664 // 'Fd'

  implicit object serializer extends ImmutableSerializer[FadeSpec] {
    def write(v: FadeSpec, out: DataOutput): Unit = {
      import v._
      out.writeShort(COOKIE)
      out.writeLong (numFrames)
      Curve.serializer.write(curve, out)
      out.writeFloat(floor)
    }

    def read(in: DataInput): FadeSpec = {
      val cookie = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val numFrames = in.readLong()
      val curve     = Curve.serializer.read(in)
      val floor     = in.readFloat()
      FadeSpec(numFrames = numFrames, curve = curve, floor = floor)
    }
  }

  object Obj extends expr.impl.ExprTypeImpl[FadeSpec, FadeSpec.Obj] {
    def typeID: Int = FadeSpec.typeID

    import FadeSpec.{Obj => Repr}

    private[this] lazy val _init: Unit = registerExtension(Apply)

    override def init(): Unit = {
      super.init()
      _init
    }

    protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
                                    (implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
      extends ConstImpl[S] with Repr[S]

    private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
      extends VarImpl[S] with Repr[S]

    def valueSerializer: ImmutableSerializer[FadeSpec] = FadeSpec.serializer

    def apply[S <: Sys[S]](numFrames: LongObj[S], shape: CurveObj[S], floor: DoubleObj[S])
                          (implicit tx: S#Tx): Obj[S] = {
      val targets = Targets[S]
      new Apply(targets, numFrames, shape, floor).connect()
    }

    def unapply[S <: Sys[S]](expr: Obj[S]): Option[(LongObj[S], CurveObj[S], DoubleObj[S])] =
      expr match {
        case impl: Apply[S] => Some((impl.numFrames, impl.shape, impl.floor))
        case _ => None
      }

    private object Apply extends Type.Extension1[Obj] {
      final val opID = 0

      def readExtension[S <: Sys[S]](opID: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                    (implicit tx: S#Tx): Obj[S] = {
        val numFrames = LongObj  .read(in, access)
        val shape     = CurveObj .read(in, access)
        val floor     = DoubleObj.read(in, access)
        new Apply(targets, numFrames, shape, floor)
      }

      def name: String = "Apply"

      val opHi: Int = opID
      val opLo: Int = opID
    }
    private final class Apply[S <: Sys[S]](protected val targets: Targets[S],
                                           val numFrames: LongObj[S],
                                           val shape: CurveObj[S],
                                           val floor: DoubleObj[S])
      extends lucre.expr.impl.NodeImpl[S, FadeSpec] with Obj[S] {

      def tpe: stm.Obj.Type = FadeSpec.Obj

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Apply(Targets[Out], context(numFrames), context(shape), context(floor)).connect()

      def value(implicit tx: S#Tx): FadeSpec = FadeSpec(numFrames.value, shape.value, floor.value.toFloat)

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[m.Change[FadeSpec]] = {
          val framesEvt = numFrames.changed
          val framesChO = if (pull.contains(framesEvt)) pull(framesEvt) else None
          val shapeEvt  = shape.changed
          val shapeChO  = if (pull.contains(shapeEvt )) pull(shapeEvt ) else None
          val floorEvt  = floor.changed
          val floorChO  = if (pull.contains(floorEvt )) pull(floorEvt ) else None

          if (framesChO.isEmpty && shapeChO.isEmpty && floorChO.isEmpty) return None

          val framesCh = framesChO.getOrElse {
            val framesV = numFrames.value
            m.Change(framesV, framesV)
          }

          val shapeCh = shapeChO.getOrElse {
            val shapeV = shape.value
            m.Change(shapeV, shapeV)
          }

          val floorCh = floorChO.getOrElse {
            val floorV = floor.value
            m.Change(floorV, floorV)
          }

          val before  = FadeSpec(framesCh.before, shapeCh.before, floorCh.before.toFloat)
          val now     = FadeSpec(framesCh.now,    shapeCh.now,    floorCh.now   .toFloat)

          Some(m.Change(before, now))
        }
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(Apply.opID)
        numFrames.write(out)
        shape    .write(out)
        floor    .write(out)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        numFrames.changed ---> changed
        shape    .changed ---> changed
        floor    .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: S#Tx): Unit = {
        numFrames.changed -/-> changed
        shape    .changed -/-> changed
        floor    .changed -/-> changed
      }
    }
  }
  trait Obj[S <: Sys[S]] extends _Expr[S, FadeSpec]
}
final case class FadeSpec(numFrames: Long, curve: Curve = linear, floor: Float = 0f)
