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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Form, MapLike, Sys}

import SourcesAsRunnerMap.Map

object SourcesAsRunnerMap {
  type Map[S <: Sys[S]] = scala.collection.immutable.Map[String, Either[stm.Source[S#Tx, Form[S]], Form[S]]]
}
/** A runner map with constant keys, and values being either `stm.Obj` sources or forms such as `Const.Expanded`
  */
final class SourcesAsRunnerMap[S <: Sys[S]](map: Map[S])
  extends MapLike[S, String, Form] {

  def isEmpty (implicit tx: S#Tx): Boolean = map.isEmpty
  def nonEmpty(implicit tx: S#Tx): Boolean = map.nonEmpty

  def changed: Observable[S#Tx, MapLike.Update[S, String, Form]] =
    Observable.empty

  def contains(key: String)(implicit tx: S#Tx): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: S#Tx): Option[Form[S]] =
    map.get(key).map {
      case Left(source) => source()
      case Right(form)  => form
    }

  def dispose()(implicit tx: S#Tx): Unit = ()
}
