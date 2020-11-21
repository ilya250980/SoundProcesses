/*
 *  FadeSpec.scala
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

package de.sciss.proc

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.expr.graph.{Ex, FadeSpec => _FadeSpec}
import de.sciss.lucre.impl.{ExprNodeImpl, ExprTypeImpl}
import de.sciss.lucre.{Copy, DoubleObj, Elem, Ident, LongObj, Pull, Txn, Expr => _Expr, Obj => LObj, Var => LVar}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}
import de.sciss.synth.Curve
import de.sciss.synth.Curve.linear
import de.sciss.{model => m}

object FadeSpec {
  final val typeId = 14

  /* override */ def init(): Unit = {
    // super.init()
    Obj  .init()
  }

  private final val COOKIE = 0x4664 // 'Fd'

  implicit object format extends ConstFormat[FadeSpec] {
    def write(v: FadeSpec, out: DataOutput): Unit = {
      import v._
      out.writeShort(COOKIE)
      out.writeLong (numFrames)
      Curve.format.write(curve, out)
      out.writeFloat(floor)
    }

    def read(in: DataInput): FadeSpec = {
      val cookie = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val numFrames = in.readLong()
      val curve     = Curve.format.read(in)
      val floor     = in.readFloat()
      FadeSpec(numFrames = numFrames, curve = curve, floor = floor)
    }
  }

  object Obj extends ExprTypeImpl[FadeSpec, FadeSpec.Obj] {
    def typeId: Int = FadeSpec.typeId

    import FadeSpec.{Obj => Repr}

    def tryParse(value: Any): Option[FadeSpec] = value match {
      case x: FadeSpec  => Some(x)
      case _            => None
    }

    private[this] lazy val _init: Unit = registerExtension(Apply)

    override def init(): Unit = {
      super.init()
      _init
    }

    protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
      new _Const[T](id, value)

    protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                    (implicit tx: T): Var[T] = {
      val res = new _Var[T](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
      extends ConstImpl[T] with Repr[T]

    private final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
      extends VarImpl[T] with Repr[T]

    def valueFormat: ConstFormat[FadeSpec] = FadeSpec.format

    def apply[T <: Txn[T]](numFrames: LongObj[T], shape: CurveObj[T], floor: DoubleObj[T])
                          (implicit tx: T): Obj[T] =
      Apply(numFrames, shape, floor)

    def unapply[T <: Txn[T]](expr: Obj[T]): Option[(LongObj[T], CurveObj[T], DoubleObj[T])] =
      expr match {
        case impl: Apply[T] => Some((impl.numFrames, impl.shape, impl.floor))
        case _ => None
      }

    object Apply extends _Expr.Type.Extension1[Obj] {
      final val opId = 0

      def apply[T <: Txn[T]](numFrames: LongObj[T], shape: CurveObj[T], floor: DoubleObj[T])
                            (implicit tx: T): Obj[T] = {
        val targets = Targets[T]()
        new ApplyImpl(targets, numFrames, shape, floor).connect()
      }

      def unapply[T <: Txn[T]](obj: Apply[T]): Option[(LongObj[T], CurveObj[T], DoubleObj[T])] =
        Some((obj.numFrames, obj.shape, obj.floor))

      def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                    (implicit tx: T): Obj[T] = {
        val numFrames = LongObj  .read[T](in)
        val shape     = CurveObj .read[T](in)
        val floor     = DoubleObj.read[T](in)
        new ApplyImpl(targets, numFrames, shape, floor)
      }

      def name: String = "Apply"

      val opHi: Int = opId
      val opLo: Int = opId
    }
    trait Apply[T <: Txn[T]] extends Obj[T] {
      def numFrames : LongObj[T]
      def shape     : CurveObj[T]
      def floor     : DoubleObj[T]
    }

    private final class ApplyImpl[T <: Txn[T]](protected val targets: Targets[T],
                                           val numFrames: LongObj[T],
                                           val shape: CurveObj[T],
                                           val floor: DoubleObj[T])
      extends ExprNodeImpl[T, FadeSpec] with Apply[T] {

      def tpe: LObj.Type = FadeSpec.Obj

      def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
        new ApplyImpl(Targets[Out](), context(numFrames), context(shape), context(floor)).connect()

      def value(implicit tx: T): FadeSpec = FadeSpec(numFrames.value, shape.value, floor.value.toFloat)

      object changed extends Changed {
        def pullUpdate(pull: Pull[T])(implicit tx: T): Option[m.Change[FadeSpec]] = {
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

      protected def disposeData()(implicit tx: T): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(Apply.opId)
        numFrames.write(out)
        shape    .write(out)
        floor    .write(out)
      }

      def connect()(implicit tx: T): this.type = {
        numFrames.changed ---> changed
        shape    .changed ---> changed
        floor    .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: T): Unit = {
        numFrames.changed -/-> changed
        shape    .changed -/-> changed
        floor    .changed -/-> changed
      }
    }
  }
  trait Obj[T <: Txn[T]] extends _Expr[T, FadeSpec]

  implicit final class ExOps(private val x: Ex[FadeSpec]) extends AnyVal {
    def numFrames : Ex[Long]   = _FadeSpec.NumFrames(x)
    def curve     : Ex[Curve]  = _FadeSpec.Curve    (x)
    def floor     : Ex[Double] = _FadeSpec.Floor    (x)
  }

  implicit object ExValue extends Ex.Value[FadeSpec]
}
final case class FadeSpec(numFrames: Long, curve: Curve = linear, floor: Float = 0f) // 1.0E-4f)
