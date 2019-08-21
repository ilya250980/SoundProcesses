/*
 *  SourcesAsRunnerMap.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Form, MapLike, Sys}

final class SourcesAsRunnerMap[S <: Sys[S]](map: Map[String, stm.Source[S#Tx, Form[S]]])
  extends MapLike[S, String, Form] {

  def isEmpty (implicit tx: S#Tx): Boolean = map.isEmpty
  def nonEmpty(implicit tx: S#Tx): Boolean = map.nonEmpty

  def changed: Observable[S#Tx, MapLike.Update[S, String, Form]] =
    Observable.empty

  def contains(key: String)(implicit tx: S#Tx): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: S#Tx): Option[V] =
    map.get(key).map(_.apply())

  def dispose()(implicit tx: S#Tx): Unit = ()
}
