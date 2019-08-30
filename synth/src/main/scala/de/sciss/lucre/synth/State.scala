/*
 *  State.scala
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

package de.sciss.lucre.synth

import scala.concurrent.stm.Ref

object State {
  def apply(owner: Any, name: String, init: Boolean): State = new Impl(owner, name, init)

  private final class Impl(val owner: Any, val name: String, init: Boolean) extends State {
    val value: Ref[Boolean] = Ref(init)

    def swap(newValue: Boolean)(implicit tx: Txn): Boolean = value.swap(newValue)(tx.peer)

    def get(implicit tx: Txn): Boolean = value.get(tx.peer)
  }
}
sealed trait State {
  protected def value: Ref[Boolean]

  def swap(newValue: Boolean)(implicit tx: Txn): Boolean

  def get(implicit tx: Txn): Boolean

  final def set(newValue: Boolean)(implicit tx: Txn): Unit =
    value.set(newValue)(tx.peer)

  protected def owner: Any

  def name: String

  override def toString = s"<$owner $name>"
}