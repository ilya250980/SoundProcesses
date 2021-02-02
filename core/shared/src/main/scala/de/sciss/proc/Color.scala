/*
 *  Color.scala
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

package de.sciss.proc

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.Ex
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.lucre.{Ident, Txn, Expr => Epr, Var => LVar}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}

import scala.collection.immutable.{IndexedSeq => Vec}

object Color {
  final val typeId = 22

  private final val COOKIE = 0x436F // 'Co'

  def init(): Unit = Obj.init()

  implicit object format extends ConstFormat[Color] {
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

    implicit def valueFormat: ConstFormat[Color] = Color.format

    def tryParse(value: Any): Option[Color] = value match {
      case x: Color => Some(x)
      case _        => None
    }

    protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
      new _Const[T](id, value)

    protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                    (implicit tx: T): Var[T] = {
      val res = new _Var[T](targets, vr)
      if (connect) res.connect()
      res
    }

    private[this] final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
      extends ConstImpl[T] with Repr[T]

    private[this] final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
      extends VarImpl[T] with Repr[T]
  }
  sealed trait Obj[T <: Txn[T]] extends Epr[T, Color]

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

  object Predefined extends ProductReader[Predefined] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Predefined = {
      require (arity == 3 && adj == 0)
      val _id   = in.readInt()
      val _name = in.readString()
      val _rgba = in.readInt()
      new Predefined(_id, _name, _rgba)
    }
  }
  final case class Predefined private[Color] (id: Int, name: String, rgba: Int) extends Color {
    override def productPrefix: String = s"Color$$Predefined" // serialization
  }

  object User extends ProductReader[User] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): User = {
      require (arity == 1 && adj == 0)
      val _rgba = in.readInt()
      new User(_rgba)
    }
  }
  final case class User(rgba: Int) extends Color {
    def name  = "User"
    def id    = 16

    override def productPrefix: String = s"Color$$User" // serialization
  }

  // XXX TODO why do we need this?
  implicit object ExValue extends Ex.Value[Color]
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
