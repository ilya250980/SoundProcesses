//package de.sciss.lucre.stm
//
//import de.sciss.lucre.{DurableLike, Ident, InMemoryLike, Txn, confluent}
//
//// XXX TODO - perhaps this should become public API?
//object IdPeek {
//  def apply[T <: Txn[T]](id: Ident[T]): Int = id match {
//    case x: InMemoryLike .Id[_] => x.id
//    case x: DurableLike  .Id[_] => x.id
//    case x: confluent.Ident[_] => x.base
//    case _ => sys.error(s"Unsupported identifier $id")
//  }
//}
