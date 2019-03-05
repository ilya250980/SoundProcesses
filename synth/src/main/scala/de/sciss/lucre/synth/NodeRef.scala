/*
 *  NodeRef.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.synth.ControlSet
import de.sciss.topology

object NodeRef {
  trait Full[S <: stm.Sys[S]] extends NodeRef with Disposable[S#Tx] {

    /** Adds a user to the node-ref. If it is already playing,
      * it successively calls `user.add()`.
      */
    def addUser(user: DynamicUser)(implicit tx: Txn): Unit

    /** Removes a user from the node-ref. __Note:__ If the node-ref
      * is already playing, it currently does not call `user.remove()`,
      * but this must be done by the caller.
      * XXX TODO -- perhaps we should change that?
      */
    def removeUser(user: DynamicUser)(implicit tx: Txn): Unit

    def addResource   (resource: Resource)(implicit tx: Txn): Unit
    def removeResource(resource: Resource)(implicit tx: Txn): Unit

    def addControl(pair: ControlSet)(implicit tx: S#Tx): Unit
  }

  final case class Edge(source: NodeRef, sink: NodeRef)
    extends topology.Edge[NodeRef] {

    def sourceVertex: NodeRef = source
    def targetVertex: NodeRef = sink
  }
}
trait NodeRef {
  def server: Server
  def node(implicit tx: Txn): Node
}