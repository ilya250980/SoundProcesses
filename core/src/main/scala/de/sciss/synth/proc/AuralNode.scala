/*
 *  AuralNode.scala
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

import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{Group, NodeRef, Server, Txn, Sys => SSys}
import de.sciss.synth.NestedUGenGraphBuilder
import de.sciss.synth.proc.impl.{AuralNodeImpl => Impl}

object AuralNode {
  def apply[S <: SSys[S]](timeRef: TimeRef, wallClock: Long, ubRes: NestedUGenGraphBuilder.Result,
                          server: Server, nameHint: Option[String])(implicit tx: Txn): Builder[S] =
    Impl[S](timeRef, wallClock, ubRes, server, nameHint = nameHint)

  trait Builder[S <: Sys[S]] extends AuralNode[S] {
    def play()(implicit tx: S#Tx): Unit
  }
}

trait AuralNode[S <: Sys[S]] extends NodeRef.Full[S] {
  def timeRef: TimeRef

  def shiftTo(newWallClock: Long): TimeRef

  /** Retrieves the main group of the Proc, or returns None if a group has not yet been assigned. */
  def groupOption(implicit tx: Txn): Option[Group]

  /** Retrieves the main group of the Proc. If this group has not been assigned yet,
    * this method will create a new group. */
  def group()(implicit tx: S#Tx): Group

  def group_=(value: Group)(implicit tx: S#Tx): Unit

  def preGroup()(implicit tx: S#Tx): Group
}