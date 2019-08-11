/*
 *  Color.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.impl.ExprTypeImpl
import de.sciss.lucre.expr.{Expr => _Expr}
import de.sciss.lucre.stm.Sys
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}

import scala.collection.immutable.{IndexedSeq => Vec}

object Color {
  final val typeId = 22

  private final val COOKIE = 0x436F // 'Co'

  def init(): Unit = Obj.init()

  implicit object serializer extends ImmutableSerializer[Color] {
    def write(c: Color, out: DataOutput): Unit = {
      out.writeShort(COOKIE)
      out.writeByte(c.id)
      if (c.id >= 16) out.writeInt(c.rgba)
    }

    def read(in: DataInput): Color = {
      val cookie = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val id = in.readByte()
      if (id < 16) Palette(id)
      else User(in.readInt())
    }
  }

  object Obj extends ExprTypeImpl[Color, Obj] {
    import Color.{Obj => Repr}

    def typeId: Int = Color.typeId

    implicit def valueSerializer: ImmutableSerializer[Color] = Color.serializer

    protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                    (implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private[this] final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
      extends ConstImpl[S] with Repr[S]

    private[this] final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
      extends VarImpl[S] with Repr[S]
  }
  sealed trait Obj[S <: Sys[S]] extends _Expr[S, Color]

  /** Palette of sixteen predefined colors. */
  val Palette: Vec[Color] = Vector(
    Predefined( 0, "Dark Blue"  , rgba = 0xFF00235C),
    Predefined( 1, "Light Blue" , rgba = 0xFF007EFA),
    Predefined( 2, "Cyan"       , rgba = 0xFF62F2F5),
    Predefined( 3, "Mint"       , rgba = 0xFF34AC71),
    Predefined( 4, "Green"      , rgba = 0xFF3CEA3B),
    Predefined( 5, "Yellow"     , rgba = 0xFFEEFF00),
    Predefined( 6, "Dark Beige" , rgba = 0xFF7D654B),
    Predefined( 7, "Light Beige", rgba = 0xFFAA9B72),
    Predefined( 8, "Orange"     , rgba = 0xFFFF930D),
    Predefined( 9, "Red"        , rgba = 0xFFFF402E),
    Predefined(10, "Maroon"     , rgba = 0xFF8D0949),
    Predefined(11, "Fuchsia"    , rgba = 0xFFFF06D8),
    Predefined(12, "Purple"     , rgba = 0xFFBC00E6),
    Predefined(13, "Black"      , rgba = 0xFF000000),
    Predefined(14, "Silver"     , rgba = 0xFFA9BBC0),
    Predefined(15, "White"      , rgba = 0xFFFFFFFF)
  )

  private final case class Predefined(id: Int, name: String, rgba: Int) extends Color

  final case class User(rgba: Int) extends Color {
    def name = "User"
    def id = 16
  }
}
sealed trait Color extends Product {
  /** Value consisting of the alpha component in bits 24-31, the red component in bits 16-23,
    * the green component in bits 8-15, and the blue component in bits 0-7.
    *
    * So technically the bits are sorted as 'ARGB'
    */
  def rgba: Int

  /** The identifier is used for serialization. Predefined
    * colors have an id smaller than 16, user colors have an id of 16.
    */
  def id: Int

  /** Either predefined name or `"User"` */
  def name: String
}
