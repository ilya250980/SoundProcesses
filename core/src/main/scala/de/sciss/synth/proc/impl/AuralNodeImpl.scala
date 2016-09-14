/*
 *  AuralNodeImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{DynamicUser, Group, Node, Resource, Synth, Sys, Txn}
import de.sciss.synth.{ControlSet, addBefore, addToHead}

import scala.concurrent.stm.Ref

object AuralNodeImpl {
  def apply[S <: Sys[S]](timeRef: TimeRef, wallClock: Long, node: Node)(implicit tx: Txn): AuralNode.Builder[S] = {
    // XXX TODO -- probably we can throw `users` and `resources` together as disposables
    val res = new Impl[S](timeRef, wallClock, node)
    node.server.addVertex(res)
    res
  }

  /*
   * The possible differentiation of groups for an aural process. The minimum configuration is one main
   * group. If synths need to play before the main process, a pre group will be established, if they need
   * to play after the main process, a post group will be established. If multiple synths participate in
   * the main process, a core group may be established. A back group will hold 'forgotten' synths, once
   * they have been marked to fade out and be removed by themselves.
   */
  private final case class AllGroups(main: Group, pre: Option[Group] = None,
                                     core: Option[Group] = None,
                                     post: Option[Group] = None, back: Option[Group] = None)

  private final class Impl[S <: Sys[S]](val timeRef: TimeRef, wallClock: Long, graphMain: Node)
    extends AuralNode.Builder[S] {

    import TxnLike.peer

    private[this] val users       = Ref(List.empty[DynamicUser])
    private[this] val resources   = Ref(List.empty[Resource   ])
    private[this] val groupsRef   = Ref(Option.empty[AllGroups])

    // we only add to `setMap` before `play`, thus does not need to be transactional
    private[this] var setMap    = List.empty[ControlSet]

    override def toString = s"AuralProc($graphMain)"

    def server = graphMain.server

    def groupOption(implicit tx: Txn): Option[Group] = groupsRef().map(_.main)

    def node(implicit tx: Txn): Node = groupOption.getOrElse(graphMain)

    def play()(implicit tx: S#Tx): Unit = {
      // `play` calls `requireOffline`, so we are safe against accidental repeated calls
      val target = server.defaultGroup
      ??? // graphMain.play(target = target, addAction = addToHead, args = setMap.reverse, dependencies = resources().reverse)
      users().reverse.foreach(_.add())
    }

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    def group()(implicit tx: S#Tx): Group =
      groupOption.getOrElse {
        val res = Group(graphMain, addBefore) // i.e. occupy the same place as before
        group_=(res)
        res
      }

    def group_=(newGroup: Group)(implicit tx: S#Tx): Unit =
      groupsRef.transform { groupsOpt =>
        val res = groupsOpt.fold {
          val all = AllGroups(main = newGroup)
          graphMain.moveToHead(newGroup)
          all
        } { all =>
          moveAllTo(all, newGroup)
          all.main.free() // what can you do...?
          all.copy(main = newGroup)
        }
        Some(res)
      }

    @inline
    private[this] def preGroupOption(implicit tx: S#Tx): Option[Group] =
      groupsRef.get(tx.peer).flatMap(_.pre)

    def preGroup()(implicit tx: S#Tx): Group =
      preGroupOption.getOrElse {
        /* val main = */ group() // creates group if necessary
        val all       = groupsRef().get
        val target    = anchorNode()
        val addAction = addBefore
        val res       = Group(target = target, addAction = addAction)
        groupsRef.set(Some(all.copy(pre = Some(res))))
        res
      }

    private def anchorNode()(implicit tx: S#Tx): Node =
      groupsRef().flatMap(_.core) getOrElse graphMain

    private def moveAllTo(all: AllGroups, newGroup: Group)(implicit tx: S#Tx): Unit = {
      val core = anchorNode()
      core.moveToTail(newGroup)
      all.pre .foreach(_.moveBefore(core))
      all.post.foreach(_.moveAfter (core))

      all.back.foreach { g =>
        if (g.isOnline) g.moveToHead(newGroup)
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      node.free()
      users      .swap(Nil).reverse.foreach(_.dispose())
      resources  .swap(Nil).reverse.foreach(_.dispose())
      // disposables.swap(Nil).reverse.foreach(_.dispose())
      server.removeVertex(this)
    }

    def addControl(pair: ControlSet)(implicit tx: S#Tx): Unit = {
      if (graphMain.isOnline) node.set(pair)
      else setMap ::= pair
    }

    def addUser(user: DynamicUser)(implicit tx: Txn): Unit = {
      users.transform(user :: _)
      if (graphMain.isOnline) user.add()
    }

    def removeUser(user: DynamicUser )(implicit tx: Txn): Unit =
      users.transform(_.filterNot(_ == user))

    def addResource(resource: Resource)(implicit tx: Txn): Unit =
      resources.transform(resource :: _)

    def removeResource(resource: Resource)(implicit tx: Txn): Unit =
      resources.transform(_.filterNot(_ == resource))

//    def addDisposable(d: Disposable[S#Tx])(implicit tx: S#Tx): Unit =
//      disposables.transform(d :: _)
  }
}