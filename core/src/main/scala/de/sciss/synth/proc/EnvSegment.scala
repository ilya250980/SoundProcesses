/*
 *  EnvSegment.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.{Pull, Targets}
import de.sciss.lucre.expr.{DoubleObj, DoubleVector, Expr, Type}
import de.sciss.lucre.stm.{Copy, Elem, Sys}
import de.sciss.lucre.{expr, stm}
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Writable}
import de.sciss.synth.Curve
import de.sciss.synth.UGenSource.Vec
import de.sciss.synth.ugen.ControlValues
import de.sciss.{lucre, model => m}

import scala.annotation.switch

object EnvSegment {
  final val typeId = 30

  def init(): Unit = Obj.init()

  private final val COOKIE = 0x5367 // 'Sg'

  implicit object serializer extends ImmutableSerializer[EnvSegment] {
    def write(v: EnvSegment, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput): EnvSegment = {
      val cookie      = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val tpe         = in.readByte()
      (tpe: @switch) match {
        case 0 =>
          val startLevel    = in.readDouble()
          val curve         = Curve.serializer.read(in)
          EnvSegment.Single(startLevel = startLevel, curve = curve)
        case 1 =>
          val startLevels   = DoubleVector.valueSerializer.read(in)
          val curve         = Curve.serializer.read(in)
          EnvSegment.Multi(startLevels = startLevels, curve = curve)
        case _ => sys.error(s"Unexpected segment type $tpe")
      }
    }
  }

  object Obj extends expr.impl.ExprTypeImpl[EnvSegment, EnvSegment.Obj] {
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

    protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                    (implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
      extends ConstImpl[S] with Repr[S]

    private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
      extends VarImpl[S] with Repr[S]

    def valueSerializer: ImmutableSerializer[EnvSegment] = EnvSegment.serializer

    def apply[S <: Sys[S]](startLevel: DoubleObj[S], curve: CurveObj[S])
                          (implicit tx: S#Tx): Obj[S] = ApplySingle(startLevel, curve)

    object ApplySingle extends Type.Extension1[Obj] {
      final val opId = 0

      def apply[S <: Sys[S]](startLevel: DoubleObj[S], curve: CurveObj[S])
                            (implicit tx: S#Tx): Obj[S] = {
        val targets = Targets[S]
        new ApplySingle(targets, startLevel = startLevel, curve = curve).connect()
      }

      def unapply[S <: Sys[S]](expr: Obj[S]): Option[(DoubleObj[S], CurveObj[S])] =
        expr match {
          case impl: ApplySingle[S] => Some((impl.startLevel, impl.curve))
          case _ => None
        }

      def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                    (implicit tx: S#Tx): Obj[S] = {
        val startLevel  = DoubleObj .read(in, access)
        val curve       = CurveObj  .read(in, access)
        new ApplySingle(targets, startLevel, curve)
      }

      def name: String = "ApplySingle"

      val opHi: Int = opId
      val opLo: Int = opId
    }
    private final class ApplySingle[S <: Sys[S]](protected val  targets     : Targets   [S],
                                                 val            startLevel  : DoubleObj [S],
                                                 val            curve       : CurveObj  [S])
      extends lucre.expr.impl.NodeImpl[S, EnvSegment.Single] with Obj[S] {

      def tpe: stm.Obj.Type = EnvSegment.Obj

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new ApplySingle(Targets[Out], context(startLevel), context(curve)).connect()

      def value(implicit tx: S#Tx): EnvSegment.Single = EnvSegment.Single(startLevel.value, curve.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[m.Change[EnvSegment.Single]] = {
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

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(ApplySingle.opId)
        startLevel .write(out)
        curve       .write(out)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        startLevel .changed ---> changed
        curve       .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: S#Tx): Unit = {
        startLevel .changed -/-> changed
        curve       .changed -/-> changed
      }
    }

    object ApplyMulti extends Type.Extension1[Obj] {
      final val opId = 1

      def apply[S <: Sys[S]](startLevels: DoubleVector[S], curve: CurveObj[S])
                            (implicit tx: S#Tx): Obj[S] = {
        val targets = Targets[S]
        new ApplyMulti(targets, startLevels = startLevels, curve = curve).connect()
      }

      def unapply[S <: Sys[S]](expr: Obj[S]): Option[(DoubleVector[S], CurveObj[S])] =
        expr match {
          case impl: ApplyMulti[S] => Some((impl.startLevels, impl.curve))
          case _ => None
        }

      def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                    (implicit tx: S#Tx): Obj[S] = {
        val startLevels = DoubleVector.read(in, access)
        val curve       = CurveObj    .read(in, access)
        new ApplyMulti(targets, startLevels, curve)
      }

      def name: String = "ApplyMulti"

      val opHi: Int = opId
      val opLo: Int = opId
    }
    private final class ApplyMulti[S <: Sys[S]](protected val targets     : Targets      [S],
                                                val           startLevels : DoubleVector [S],
                                                val           curve       : CurveObj     [S])
      extends lucre.expr.impl.NodeImpl[S, EnvSegment.Multi] with Obj[S] {

      def tpe: stm.Obj.Type = EnvSegment.Obj

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new ApplyMulti(Targets[Out], context(startLevels), context(curve)).connect()

      def value(implicit tx: S#Tx): EnvSegment.Multi = EnvSegment.Multi(startLevels.value, curve.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[m.Change[EnvSegment.Multi]] = {
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

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(ApplyMulti.opId)
        startLevels.write(out)
        curve      .write(out)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        startLevels.changed ---> changed
        curve      .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: S#Tx): Unit = {
        startLevels.changed -/-> changed
        curve      .changed -/-> changed
      }
    }
  }
  trait Obj[S <: Sys[S]] extends Expr[S, EnvSegment]

  final case class Single(startLevel: Double, curve: Curve) extends EnvSegment {
    def numChannels: Int = 1

    def updateCurve(curve: Curve): EnvSegment = copy(curve = curve)

    def startLevels: Vec[Double] = Vector(startLevel)

    private[proc] def startLevelsAsControl: ControlValues = startLevel

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      out.writeByte(0)
      out.writeDouble(startLevel)
      Curve.serializer.write(curve, out)
    }
  }
  final case class Multi(startLevels: Vec[Double], curve: Curve) extends EnvSegment {
    def numChannels: Int = startLevels.size

    def updateCurve(curve: Curve): EnvSegment = copy(curve = curve)

    private[proc] def startLevelsAsControl: ControlValues = startLevels.map(_.toFloat)

    def write(out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      out.writeByte(1)
      DoubleVector.valueSerializer.write(startLevels, out)
      Curve.serializer.write(curve, out)
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