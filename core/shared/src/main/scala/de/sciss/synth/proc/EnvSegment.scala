/*
 *  EnvSegment.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{ExprNodeImpl, ExprTypeImpl}
import de.sciss.lucre.{Copy, DoubleObj, DoubleVector, Elem, Expr, Ident, Pull, Txn, Obj => LObj, Var => LVar}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, Writable}
import de.sciss.synth.Curve
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.ugen.ControlValues
import de.sciss.{model => m}

import scala.annotation.switch

object EnvSegment {
  final val typeId = 30

  def init(): Unit = Obj.init()

  private final val COOKIE = 0x5367 // 'Sg'

  implicit object format extends ConstFormat[EnvSegment] {
    def write(v: EnvSegment, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput): EnvSegment = {
      val cookie      = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val tpe         = in.readByte()
      (tpe: @switch) match {
        case 0 =>
          val startLevel    = in.readDouble()
          val curve         = Curve.format.read(in)
          EnvSegment.Single(startLevel = startLevel, curve = curve)
        case 1 =>
          val startLevels   = DoubleVector.valueFormat.read(in)
          val curve         = Curve.format.read(in)
          EnvSegment.Multi(startLevels = startLevels, curve = curve)
        case _ => sys.error(s"Unexpected segment type $tpe")
      }
    }
  }

  object Obj extends ExprTypeImpl[EnvSegment, EnvSegment.Obj] {
    def typeId: Int = EnvSegment.typeId

    import EnvSegment.{Obj => Repr}

    def tryParse(value: Any): Option[EnvSegment] = value match {
      case x: EnvSegment  => Some(x)
      case _              => None
    }

    private[this] lazy val _init: Unit = {
      registerExtension(ApplySingle)
      registerExtension(ApplyMulti )
    }

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

    def valueFormat: ConstFormat[EnvSegment] = EnvSegment.format

    def apply[T <: Txn[T]](startLevel: DoubleObj[T], curve: CurveObj[T])
                          (implicit tx: T): Obj[T] = ApplySingle(startLevel, curve)

    object ApplySingle extends Expr.Type.Extension1[Obj] {
      final val opId = 0

      def apply[T <: Txn[T]](startLevel: DoubleObj[T], curve: CurveObj[T])
                            (implicit tx: T): Obj[T] = {
        val targets = Targets[T]()
        new ApplySingle(targets, startLevel = startLevel, curve = curve).connect()
      }

      def unapply[T <: Txn[T]](expr: Obj[T]): Option[(DoubleObj[T], CurveObj[T])] =
        expr match {
          case impl: ApplySingle[T] => Some((impl.startLevel, impl.curve))
          case _ => None
        }

      def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                    (implicit tx: T): Obj[T] = {
        val startLevel  = DoubleObj .read(in)
        val curve       = CurveObj  .read(in)
        new ApplySingle(targets, startLevel, curve)
      }

      def name: String = "ApplySingle"

      val opHi: Int = opId
      val opLo: Int = opId
    }
    private final class ApplySingle[T <: Txn[T]](protected val  targets     : Targets   [T],
                                                 val            startLevel  : DoubleObj [T],
                                                 val            curve       : CurveObj  [T])
      extends ExprNodeImpl[T, EnvSegment.Single] with Obj[T] {

      def tpe: LObj.Type = EnvSegment.Obj

      def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
        new ApplySingle(Targets[Out](), context(startLevel), context(curve)).connect()

      def value(implicit tx: T): EnvSegment.Single = EnvSegment.Single(startLevel.value, curve.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[T])(implicit tx: T): Option[m.Change[EnvSegment.Single]] = {
          val levelEvt  = startLevel.changed
          val levelChO  = if (pull.contains(levelEvt)) pull(levelEvt) else None
          val curveEvt  = curve.changed
          val curveChO  = if (pull.contains(curveEvt)) pull(curveEvt) else None

          if (levelChO.isEmpty && curveChO.isEmpty) return None

          val levelCh = levelChO.getOrElse {
            val levelV = startLevel.value
            m.Change(levelV, levelV)
          }

          val curveCh = curveChO.getOrElse {
            val curveV = curve.value
            m.Change(curveV, curveV)
          }

          val before  = EnvSegment.Single(levelCh.before, curveCh.before)
          val now     = EnvSegment.Single(levelCh.now,    curveCh.now   )

          Some(m.Change(before, now))
        }
      }

      protected def disposeData()(implicit tx: T): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(ApplySingle.opId)
        startLevel .write(out)
        curve       .write(out)
      }

      def connect()(implicit tx: T): this.type = {
        startLevel .changed ---> changed
        curve       .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: T): Unit = {
        startLevel .changed -/-> changed
        curve       .changed -/-> changed
      }
    }

    object ApplyMulti extends Expr.Type.Extension1[Obj] {
      final val opId = 1

      def apply[T <: Txn[T]](startLevels: DoubleVector[T], curve: CurveObj[T])
                            (implicit tx: T): Obj[T] = {
        val targets = Targets[T]()
        new ApplyMulti(targets, startLevels = startLevels, curve = curve).connect()
      }

      def unapply[T <: Txn[T]](expr: Obj[T]): Option[(DoubleVector[T], CurveObj[T])] =
        expr match {
          case impl: ApplyMulti[T] => Some((impl.startLevels, impl.curve))
          case _ => None
        }

      def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                    (implicit tx: T): Obj[T] = {
        val startLevels = DoubleVector.read(in)
        val curve       = CurveObj    .read(in)
        new ApplyMulti(targets, startLevels, curve)
      }

      def name: String = "ApplyMulti"

      val opHi: Int = opId
      val opLo: Int = opId
    }
    private final class ApplyMulti[T <: Txn[T]](protected val targets     : Targets      [T],
                                                val           startLevels : DoubleVector [T],
                                                val           curve       : CurveObj     [T])
      extends ExprNodeImpl[T, EnvSegment.Multi] with Obj[T] {

      def tpe: LObj.Type = EnvSegment.Obj

      def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
        new ApplyMulti(Targets[Out](), context(startLevels), context(curve)).connect()

      def value(implicit tx: T): EnvSegment.Multi = EnvSegment.Multi(startLevels.value, curve.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[T])(implicit tx: T): Option[m.Change[EnvSegment.Multi]] = {
          val levelEvt  = startLevels.changed
          val levelChO  = if (pull.contains(levelEvt)) pull(levelEvt) else None
          val curveEvt  = curve.changed
          val curveChO  = if (pull.contains(curveEvt)) pull(curveEvt) else None

          if (levelChO.isEmpty && curveChO.isEmpty) return None

          val levelCh = levelChO.getOrElse {
            val levelV = startLevels.value
            m.Change(levelV, levelV)
          }

          val curveCh = curveChO.getOrElse {
            val curveV = curve.value
            m.Change(curveV, curveV)
          }

          val before  = EnvSegment.Multi(levelCh.before, curveCh.before)
          val now     = EnvSegment.Multi(levelCh.now,    curveCh.now   )

          Some(m.Change(before, now))
        }
      }

      protected def disposeData()(implicit tx: T): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(ApplyMulti.opId)
        startLevels.write(out)
        curve      .write(out)
      }

      def connect()(implicit tx: T): this.type = {
        startLevels.changed ---> changed
        curve      .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: T): Unit = {
        startLevels.changed -/-> changed
        curve      .changed -/-> changed
      }
    }
  }
  trait Obj[T <: Txn[T]] extends Expr[T, EnvSegment]

  final case class Single(startLevel: Double, curve: Curve) extends EnvSegment {
    def numChannels: Int = 1

    def updateCurve(curve: Curve): EnvSegment = copy(curve = curve)

    def startLevels: Vec[Double] = Vector(startLevel)

    private[proc] def startLevelsAsControl: ControlValues = startLevel

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      out.writeByte(0)
      out.writeDouble(startLevel)
      Curve.format.write(curve, out)
    }
  }
  final case class Multi(startLevels: Vec[Double], curve: Curve) extends EnvSegment {
    def numChannels: Int = startLevels.size

    def updateCurve(curve: Curve): EnvSegment = copy(curve = curve)

    private[proc] def startLevelsAsControl: ControlValues = startLevels.map(_.toFloat)

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      out.writeByte(1)
      DoubleVector.valueFormat.write(startLevels, out)
      Curve.format.write(curve, out)
    }
  }
}
sealed abstract class EnvSegment extends Product with Writable {
  def curve: Curve
  def startLevels: Vec[Double]
  def numChannels: Int

  def updateCurve(curve: Curve): EnvSegment

  private[proc] def startLevelsAsControl: ControlValues
}