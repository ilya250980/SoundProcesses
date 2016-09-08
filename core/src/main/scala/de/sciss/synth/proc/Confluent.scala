/*
 *  Confluent.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package synth
package proc

import de.sciss.lucre.confluent
import de.sciss.lucre.stm.DataStore
import de.sciss.lucre.synth.{InMemory, Sys}
import de.sciss.synth.proc.impl.{ConfluentImpl => Impl}

import scala.language.implicitConversions

object Confluent {
  private type S = Confluent

  def apply(storeFactory: DataStore.Factory): S = Impl(storeFactory)

  trait Txn extends confluent.Txn[S] with Sys.Txn[S] {
//    private[proc] def durable : Durable#Tx
//    private[proc] def inMemory: InMemory#Tx
  }

//  implicit def inMemory(tx: S#Tx): InMemory#Tx = tx.inMemory
//  implicit def durable (tx: S#Tx): Durable #Tx = tx.durable
}

trait Confluent extends confluent.Sys[Confluent] with Sys[Confluent] {
  protected type S  = Confluent
  type D            = Durable
  type I            = InMemory
  type Tx           = Confluent.Txn // Sys.Txn[ S ] with ConfluentReactiveLike.Txn[ S ]
}