package de.sciss.lucre.stm

import de.sciss.lucre.confluent

// XXX TODO - perhaps this should become public API?
object IDPeek {
  def apply[S <: Sys[S]](id: S#Id): Int = id match {
    case x: InMemoryLike .Id[_] => x.id
    case x: DurableLike  .Id[_] => x.id
    case x: confluent.Identifier[_] => x.base
    case _ => sys.error(s"Unsupported identifier $id")
  }
}
