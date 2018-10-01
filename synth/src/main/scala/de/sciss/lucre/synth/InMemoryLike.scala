package de.sciss.lucre.synth

import de.sciss.lucre.stm

object InMemoryLike {
  trait Txn[S <: Sys[S]] extends stm.InMemoryLike.Txn[S] with Sys.Txn[S]
}
trait InMemoryLike[S <: InMemoryLike[S]] extends stm.InMemoryLike[S] with Sys[S] {
  type Tx <: InMemoryLike.Txn[S]
}