/*
 *  Color.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.numbers.IntFunctions
import de.sciss.synth.proc
import de.sciss.synth.proc.Color.Palette

object Color {
  def DarkBlue    : proc.Color  = Palette( 0)
  def LightBlue   : proc.Color  = Palette( 1)
  def Cyan        : proc.Color  = Palette( 2)
  def Mint        : proc.Color  = Palette( 3)
  def Green       : proc.Color  = Palette( 4)
  def Yellow      : proc.Color  = Palette( 5)
  def DarkBeige   : proc.Color  = Palette( 6)
  def LightBeige  : proc.Color  = Palette( 7)
  def Orange      : proc.Color  = Palette( 8)
  def Red         : proc.Color  = Palette( 9)
  def Maroon      : proc.Color  = Palette(10)
  def Fuchsia     : proc.Color  = Palette(11)
  def Purple      : proc.Color  = Palette(12)
  def Black       : proc.Color  = Palette(13)
  def Silver      : proc.Color  = Palette(14)
  def White       : proc.Color  = Palette(15)

  object Predef {
    /** There are sixteen predefined colors (identifiers 0 to 15). */
    def apply(id: Ex[Int]): Ex[proc.Color] = Impl(id)

    private final class Expanded[S <: Sys[S]](id: IExpr[S, Int], tx0: S#Tx)(implicit targets: ITargets[S])
      extends MappedIExpr[S, Int, proc.Color](id, tx0) {

      protected def mapValue(inValue: Int)(implicit tx: S#Tx): proc.Color = {
        val idx = IntFunctions.mod(inValue, Palette.size)
        Palette(idx)
      }
    }

    private final case class Impl(id: Ex[Int]) extends Ex[proc.Color] {
      type Repr[S <: Sys[S]] = IExpr[S, proc.Color]

      override def productPrefix: String = s"Color$$Predef" // serialization

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new Expanded(id.expand[S], tx)
      }
    }
  }
}
