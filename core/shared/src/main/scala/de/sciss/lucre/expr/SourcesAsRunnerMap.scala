/*
 *  SourcesAsRunnerMap.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.expr.SourcesAsRunnerMap.Map
import de.sciss.lucre.{Form, MapObjLike, Observable, Source, Txn}

object SourcesAsRunnerMap {
  type Map[T <: Txn[T]] = scala.collection.immutable.Map[String, Either[Source[T, Form[T]], Form[T]]]
}
/** A runner map with constant keys, and values being either `stm.Obj` sources or forms such as `Const.Expanded`
  */
final class SourcesAsRunnerMap[T <: Txn[T]](map: Map[T])
  extends MapObjLike[T, String, Form[T]] {

  def isEmpty (implicit tx: T): Boolean = map.isEmpty
  def nonEmpty(implicit tx: T): Boolean = map.nonEmpty

  def changed: Observable[T, MapObjLike.Update[String, Form[T]]] =
    Observable.empty

  def contains(key: String)(implicit tx: T): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: T): Option[Form[T]] =
    map.get(key).map {
      case Left(source) => source()
      case Right(form)  => form
    }

  def dispose()(implicit tx: T): Unit = ()
}
