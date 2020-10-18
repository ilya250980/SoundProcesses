/*
 *  AuralNode.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.synth
import de.sciss.lucre.synth.{Group, NodeRef, RT, Server, Txn}
import de.sciss.synth.NestedUGenGraphBuilder
import de.sciss.synth.proc.impl.{AuralNodeImpl => Impl}

object AuralNode {
  def apply[T <: synth.Txn[T]](timeRef: TimeRef, wallClock: Long, ubRes: NestedUGenGraphBuilder.Result,
                          server: Server, nameHint: Option[String])(implicit tx: RT): Builder[T] =
    Impl[T](timeRef, wallClock, ubRes, server, nameHint = nameHint)

  trait Builder[T <: Txn[T]] extends AuralNode[T] {
    def play()(implicit tx: T): Unit
  }
}

trait AuralNode[T <: Txn[T]] extends NodeRef.Full[T] {
  def timeRef: TimeRef

  def shiftTo(newWallClock: Long): TimeRef

  /** Retrieves the main group of the Proc, or returns None if a group has not yet been assigned. */
  def groupOption(implicit tx: RT): Option[Group]

  /** Retrieves the main group of the Proc. If this group has not been assigned yet,
    * this method will create a new group. */
  def group()(implicit tx: T): Group

  def group_=(value: Group)(implicit tx: T): Unit

  def preGroup()(implicit tx: T): Group
}