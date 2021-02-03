/*
 *  AuralNode.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.{Disposable, synth, Txn => LTxn}
import de.sciss.lucre.synth.{DynamicUser, Group, NodeRef, RT, Resource, Server, Synth}
import de.sciss.synth.{ControlSet, NestedUGenGraphBuilder}
import de.sciss.proc.impl.{AuralNodeImpl => Impl}

object AuralNode {
  def apply[T <: synth.Txn[T]](timeRef: TimeRef, wallClock: Long, ubRes: NestedUGenGraphBuilder.Result,
                          server: Server, nameHint: Option[String])(implicit tx: RT): Builder[T] =
    Impl[T](timeRef, wallClock, ubRes, server, nameHint = nameHint)

  trait Builder[T <: LTxn[T]] extends AuralNode[T] {
    def play()(implicit tx: T): Unit
  }
}

trait AuralNode[T <: LTxn[T]] extends NodeRef with Disposable[T] /* .Full[T] */ {
  def timeRef: TimeRef

  def shiftTo(newWallClock: Long): TimeRef

  /** Retrieves the main group of the Proc, or returns None if a group has not yet been assigned. */
  def groupOption(implicit tx: RT): Option[Group]

  /** Retrieves the main group of the Proc. If this group has not been assigned yet,
    * this method will create a new group. */
  def group()(implicit tx: T): Group

  def synth: Synth

  def group_=(value: Group)(implicit tx: T): Unit

  def preGroup()(implicit tx: T): Group

  // ----

  /** Adds a user to the node-ref. If it is already playing,
    * it successively calls `user.add()`.
    */
  def addUser(user: DynamicUser)(implicit tx: RT): Unit

  /** Removes a user from the node-ref. __Note:__ If the node-ref
    * is already playing, it currently does not call `user.remove()`,
    * but this must be done by the caller.
    * XXX TODO -- perhaps we should change that?
    */
  def removeUser(user: DynamicUser)(implicit tx: RT): Unit

  def addResource   (resource: Resource)(implicit tx: RT): Unit
  def removeResource(resource: Resource)(implicit tx: RT): Unit

  def addControl(pair: ControlSet)(implicit tx: T): Unit
}