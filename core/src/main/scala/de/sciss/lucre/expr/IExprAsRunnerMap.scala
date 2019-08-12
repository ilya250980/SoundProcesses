/*
 *  IExprAsRunnerMap.scala
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

import de.sciss.lucre.event.{ITargets, Observable}
import de.sciss.lucre.expr.graph.UnaryOp
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Form, MapLike, Sys}

import scala.concurrent.stm.TMap

// XXX TODO --- we currently evaluate the keys at init time and do not observe their mutations
final class IExprAsRunnerMap[S <: Sys[S]](pairs: Seq[IExpr[S, (String, _)]], tx0: S#Tx)
                                         (implicit targets: ITargets[S])
  extends MapLike[S, String, Form] {

  private[this] val map = TMap[String, IExpr[S, _]]({
    pairs.map { tupEx =>
      tupEx.value(tx0)._1 -> new UnaryOp.Expanded(UnaryOp.Tuple2_2[String, Any](), tupEx, tx0)
    }
  }: _*)

  def isEmpty(implicit tx: S#Tx): Boolean =
    map.isEmpty

  def nonEmpty(implicit tx: S#Tx): Boolean =
    map.nonEmpty

  // XXX TODO --- we currently don't update `map`
  def changed: Observable[S#Tx, MapLike.Update[S, String, Form]] = Observable.empty

  def contains(key: String)(implicit tx: S#Tx): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: S#Tx): Option[Form[S]] =
    map.get(key)

  def dispose()(implicit tx: S#Tx): Unit = {
    map.valuesIterator.foreach(_.dispose())
    // the peer values may still be used in the
    // other control program when the map is disposed! (correct?)
    // XXX TODO -- ist unsinn, die map wird ja doch nur einmal fuer die ganze control laufzeit erzeugt
//    peer.foreach(_.dispose())
    map.clear()
  }
}
