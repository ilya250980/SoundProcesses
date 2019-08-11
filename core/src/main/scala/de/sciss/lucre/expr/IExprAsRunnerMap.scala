package de.sciss.lucre.expr

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Form, MapLike, Sys}

import scala.concurrent.stm.TMap
import scala.language.higherKinds
import scala.reflect.ClassTag

final class IExprAsRunnerMap[S <: Sys[S]](peer: Seq[IExpr[S, (String, _)]], tx0: S#Tx)
  extends MapLike[S, String, Form] {

  private[this] val map = TMap(peer.map(_.value(tx0)): _*)

  def isEmpty(implicit tx: S#Tx): Boolean =
    peer.isEmpty

  def nonEmpty(implicit tx: S#Tx): Boolean =
    peer.nonEmpty

  def changed: Observable[S#Tx, MapLike.Update[S, String, Form]] = ???

  def contains(key: String)(implicit tx: S#Tx): Boolean =
    peer.value.contains(key)

  def get(key: String)(implicit tx: S#Tx): Option[Form[S]] =
    peer.value.get(key).map {
      case f: Form[_] => f.asInstanceOf[Form[S]]
      case other      => ??? // PlainForm(other)
    }

  def $[R[~ <: Sys[~]] <: Form[~]](key: String)(implicit tx: S#Tx, ct: ClassTag[R[S]]): Option[R[S]] =
    ??? // peer.value.get(key)
}
