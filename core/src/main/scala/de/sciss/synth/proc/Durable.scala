/*
 *  Durable.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.{DataStore, DurableLike}
import de.sciss.lucre.synth.{InMemory, Sys}
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.{DurableImpl => Impl}

import scala.language.implicitConversions

object Durable {
  import proc.{Durable => S}

  def apply(factory: DataStore.Factory, mainName: String = "data"): S = Impl(factory, mainName = mainName)

  def apply(mainStore: DataStore): S = Impl(mainStore)

  implicit def inMemory(tx: Durable#Tx): InMemory#Tx = tx.inMemory

  trait Txn extends Sys.Txn[Durable] with DurableLike.Txn[Durable] {
    def inMemory: InMemory#Tx
  }
}

trait Durable extends DurableLike[Durable] with Sys[Durable] {
  final type Tx = Durable.Txn // Sys.Txn[Durable] with evt.DurableLike.Txn[Durable]
  final type I  = InMemory
}