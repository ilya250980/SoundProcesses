/*
 *  NodeRefImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package impl

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth
import de.sciss.lucre.synth.NodeRef.Full
import de.sciss.synth.{ControlSet, addBefore}

import scala.concurrent.stm.Ref

object NodeRefImpl {
  def Group(name: String, in0: AuralNode)(implicit tx: Txn): NodeRef.Group = {
    val res = new GroupImpl(name, in0)
    in0.server.addVertex(res)
    res
  }
  
  def Var(init: Full): NodeRef.Var = new VarImpl(init.server, Ref(init))

  private final class VarImpl(val server: Server, ref: Ref[Full]) extends NodeRef.Var {
    import TxnLike.peer

    def apply ()       (implicit tx: Txn): Full = ref()
    def update(v: Full)(implicit tx: Txn): Unit = ref() = v

    def node(implicit tx: Txn): Node = ref().node

    def dispose()(implicit tx: Txn): Unit = ref().dispose()

    def addUser   (user: DynamicUser)(implicit tx: Txn): Unit = ref().addUser   (user)
    def removeUser(user: DynamicUser)(implicit tx: Txn): Unit = ref().removeUser(user)

    def addResource   (resource: Resource)(implicit tx: Txn): Unit = ref().addResource   (resource)
    def removeResource(resource: Resource)(implicit tx: Txn): Unit = ref().removeResource(resource)

    def addControl(pair: ControlSet)(implicit tx: Txn): Unit = ref().addControl(pair)
  }
  
  // dynamically flips between single proc and multiple procs
  // (wrapping them in one common group)
  private final class GroupImpl(name: String, in0: AuralNode) extends NodeRef.Group {
    import TxnLike.peer

    val server = in0.server

    override def toString = name

    private val instancesRef  = Ref(in0 :: Nil)
    private val nodeRef       = Ref[NodeRef](in0)

    def node(implicit tx: Txn): Node = nodeRef().node

    def addInstanceNode(n: AuralNode)(implicit tx: Txn): Unit = {
      val old = instancesRef.getAndTransform(n :: _)
      old match {
        case single :: Nil =>
          val g = synth.Group(single.node, addBefore)
          nodeRef() = g
          single.node.moveToHead(g)
          n     .node.moveToHead(g)

        case _ =>
      }
    }

    def removeInstanceNode(n: AuralNode)(implicit tx: Txn): Boolean = {
      val after = instancesRef.transformAndGet(_.filterNot(_ == n))
      after match {
        case single :: Nil =>
          val group = nodeRef.swap(single).node
          single.node.moveBefore(group)
          group.free()
          false

        case Nil  =>
          dispose()
          true

        case _ => false
      }
    }
    
    def instanceNodes(implicit tx: Txn): Iterator[AuralNode] = instancesRef().iterator

    def addUser(user: DynamicUser)(implicit tx: Txn): Unit =
      instancesRef().foreach(_.addUser(user))

    def removeUser(user: DynamicUser)(implicit tx: Txn): Unit =
      instancesRef().foreach(_.removeUser(user))

    def addResource(resource: Resource)(implicit tx: Txn): Unit =
      instancesRef().foreach(_.addResource(resource))

    def removeResource(resource: Resource)(implicit tx: Txn): Unit =
      instancesRef().foreach(_.removeResource(resource))

    def addControl(pair: ControlSet)(implicit tx: Txn): Unit =
      instancesRef().foreach(_.addControl(pair))

    def dispose()(implicit tx: Txn): Unit = {
      if (instancesRef.swap(Nil).size > 1) {
        val group = nodeRef.swap(null).node
        group.free()
      }
      server.removeVertex(this)
    }
  }
}