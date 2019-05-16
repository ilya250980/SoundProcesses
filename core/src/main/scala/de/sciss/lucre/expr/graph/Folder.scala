/*
 *  Folder.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.CellView
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.serial.Serializer

object Folder {
  implicit object Bridge extends Obj.Bridge[Folder] {
    type Repr[S <: Sys[S]] = stm.Folder[S]

    def id: Int = ???

    def mkObj[S <: Sys[S]](value: Folder)(implicit tx: S#Tx): stm.Folder[S] =
      stm.Folder()

    implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, stm.Folder[S]] =
      stm.Folder.serializer

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[Folder]] = ???
  }

  implicit final class Ops(private val f: Ex[Folder]) extends AnyVal {
    def prepend[A](elem: Ex[A])(implicit bridge: Obj.Bridge[A]): Act = ???
    def append [A](elem: Ex[A])(implicit bridge: Obj.Bridge[A]): Act = ???
    def +=     [A](elem: Ex[A])(implicit bridge: Obj.Bridge[A]): Act = append(elem)

    def size: Ex[Int] = ???

    def isEmpty : Ex[Boolean] = ???
    def nonEmpty: Ex[Boolean] = ???
  }
}
trait Folder extends Obj {
  override private[graph] def peer[S <: Sys[S]](implicit tx: S#Tx): stm.Folder[S]
}