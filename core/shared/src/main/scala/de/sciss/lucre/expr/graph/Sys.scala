/*
 *  Sys.scala
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

import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, ITargets, Txn}

/** Access to operating system functions. */
object Sys {
  private final class ExpandedProperty[T <: Txn[T]](key: IExpr[T, String], tx0: T)
                                                   (implicit targets: ITargets[T])
    extends MappedIExpr[T, String, Option[String]](key, tx0) {

    protected def mapValue(inValue: String)(implicit tx: T): Option[String] =
      sys.props.get(inValue)
  }

  /** A system property. */
  final case class Property(key: Ex[String]) extends Ex[Option[String]] {
    override def productPrefix: String = s"Sys$$Property" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[String]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ExpandedProperty[T](key.expand[T], tx)
    }
  }

  private final class ExpandedEnv[T <: Txn[T]](key: IExpr[T, String], tx0: T)
                                                   (implicit targets: ITargets[T])
    extends MappedIExpr[T, String, Option[String]](key, tx0) {

    protected def mapValue(inValue: String)(implicit tx: T): Option[String] =
      sys.env.get(inValue)
  }

  /** An environment variable. */
  final case class Env(key: Ex[String]) extends Ex[Option[String]] {
    override def productPrefix: String = s"Sys$$Env" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[String]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ExpandedEnv[T](key.expand[T], tx)
    }
  }
}
