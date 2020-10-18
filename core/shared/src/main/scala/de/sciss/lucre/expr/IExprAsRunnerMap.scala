/*
 *  IExprAsRunnerMap.scala
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

package de.sciss.lucre
package expr

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.graph.UnaryOp

import scala.concurrent.stm.TMap

// XXX TODO --- we currently evaluate the keys at init time and do not observe their mutations
final class IExprAsRunnerMap[T <: Txn[T]](pairs: Seq[IExpr[T, (String, _)]], tx0: T)
                                         (implicit targets: ITargets[T])
  extends MapObjLike[T, String, Form[T]] {

  private[this] val map = TMap[String, IExpr[T, _]]({
    pairs.map {
      // it's important to check this case, because that way we
      // don't touch the value and things like Var.Expanded remain usable
      case tupEx: ExTuple2.Expanded[T, String, _] =>
        tupEx._1.value(tx0) -> tupEx._2

      case tupEx =>
        tupEx.value(tx0)._1 -> new UnaryOp.Expanded(UnaryOp.Tuple2_2[String, Any](), tupEx, tx0)
    }
  }: _*)

  def isEmpty(implicit tx: T): Boolean =
    map.isEmpty

  def nonEmpty(implicit tx: T): Boolean =
    map.nonEmpty

  // XXX TODO --- we currently don't update `map`
  def changed: Observable[T, MapObjLike.Update[String, Form[T]]] = Observable.empty

  def contains(key: String)(implicit tx: T): Boolean =
    map.contains(key)

  def get(key: String)(implicit tx: T): Option[Form[T]] =
    map.get(key)

  def dispose()(implicit tx: T): Unit = {
    map.valuesIterator.foreach(_.dispose())
    // the peer values may still be used in the
    // other control program when the map is disposed! (correct?)
    // XXX TODO -- ist unsinn, die map wird ja doch nur einmal fuer die ganze control laufzeit erzeugt
//    peer.foreach(_.dispose())
    map.clear()
  }
}
