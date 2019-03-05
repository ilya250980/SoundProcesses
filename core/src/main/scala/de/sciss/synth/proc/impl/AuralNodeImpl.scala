/*
 *  AuralNodeImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Bus, BusNodeSetter, DynamicUser, Group, Node, Resource, Server, Synth, Sys, Txn}
import de.sciss.synth.{AddAction, ControlSet, NestedUGenGraphBuilder, addBefore, addToHead, addToTail, audio, control}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.stm.Ref

object AuralNodeImpl {
  def apply[S <: Sys[S]](timeRef: TimeRef, wallClock: Long, ubRes: NestedUGenGraphBuilder.Result,
                         server: Server, nameHint: Option[String])
                        (implicit tx: Txn): AuralNode.Builder[S] = {
    val res = prepare(ubRes, server, nameHint = nameHint)
    // XXX TODO -- probably we can throw `users` and `resources` together as disposables
    val aNode = new Impl[S](timeRef, wallClock, tree = res.tree, setMap = res.controls, users0 = res.buses)
    server.addVertex(aNode)
    aNode
  }

  // ---- transformation of NestedUGenGraphBuilder.Result into a lucre-based tree ----

  private final case class Result(tree: Tree, controls: List[ControlSet], buses: List[BusNodeSetter])

  private sealed trait Tree {
    def main: Node
    def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
            (implicit tx: Txn): Unit
  }

  private final case class Leaf(syn: Synth) extends Tree {
    def main: Node = syn
    def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
            (implicit tx: Txn): Unit =
      syn.play(target = target, args = args, addAction = addAction, dependencies = dependencies)
  }

  private final case class Branch(group: Group, syn: Synth, children: List[Tree]) extends Tree {
    def main: Node = group
    def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
            (implicit tx: Txn): Unit = {
      group.play(target = target, addAction = addAction, args = Nil, dependencies = dependencies)
      syn  .play(target = group , addAction = addToHead, args = Nil, dependencies = Nil         )
      children.foreach { ch =>
        ch .play(target = group , addAction = addToTail, args = Nil, dependencies = Nil         )
      }
      group.set(args: _*)
    }
  }

  private def prepare(res0: NestedUGenGraphBuilder.Result, s: Server, nameHint: Option[String])
                     (implicit tx: Txn): Result = {
    var ctl   = List.empty[ControlSet]
    var buses = List.empty[BusNodeSetter]

    def loop(tree: NestedUGenGraphBuilder.Result): Tree = {
      val syn         = Synth.expanded(s, tree.graph, nameHint = nameHint)
      val hasChildren = tree.children.nonEmpty

      val child: Tree = if (hasChildren) {
        val group = Group(s)
        val children = tree.children.map { cc =>
          val ccn = loop(cc)
          if (cc.id >= 0) ctl ::= NestedUGenGraphBuilder.pauseNodeCtlName(cc.id) -> ccn.main.peer.id
          ccn
        }
        Branch(group = group, syn = syn, children = children)

      } else {
        Leaf(syn)
      }

      tree.links.foreach { link =>
        val ctlName = NestedUGenGraphBuilder.linkCtlName(link.id)
        val setter  = link.rate match {
          case `audio` =>
            val bus = Bus.audio  (s, numChannels = link.numChannels)
            BusNodeSetter.readerWriter(ctlName, bus, child.main)
          case `control` =>
            val bus = Bus.control(s, numChannels = link.numChannels)
            BusNodeSetter.readerWriter(ctlName, bus, child.main)
          case other =>
            throw new IllegalArgumentException(s"Unsupported link rate $other")
        }
        buses ::= setter
      }

      child
    }

    val top = loop(res0)
    Result(tree = top, controls = ctl, buses = buses)
  }

  // ---- impl ----

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

  // we only add to `setMap` before `play`, thus does not need to be transactional. i.e.
  // if `addControl` is called later during run, the control is not store but passed directly
  // to the server.
  private final class Impl[S <: Sys[S]](val timeRef: TimeRef, wallClock: Long, tree: Tree,
                                        private[this] var setMap: List[ControlSet], users0: List[DynamicUser])
    extends AuralNode.Builder[S] {

    import TxnLike.peer

    import tree.{main => graphMain}

    private[this] val users       = Ref(users0)
    private[this] val resources   = Ref(List.empty[Resource])

    private[this] val groupsRef   = Ref[Option[AllGroups]](graphMain match {
      case g: Group => Some(AllGroups(g))
      case _        => None
    })

    override def toString = s"AuralProc($graphMain)"

    def server: Server = graphMain.server

    def groupOption(implicit tx: Txn): Option[Group] = groupsRef().map(_.main)

    def node(implicit tx: Txn): Node = groupOption.getOrElse(graphMain)

    def play()(implicit tx: S#Tx): Unit = {
      // `play` calls `requireOffline`, so we are safe against accidental repeated calls
      val target = server.defaultGroup
      tree.play(target = target, addAction = addToHead, args = setMap.reverse, dependencies = resources().reverse)
      users().reverse.foreach(_.add())
    }

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    def group()(implicit tx: S#Tx): Group =
      groupOption.getOrElse {
        val res = Group.play(graphMain, addBefore) // i.e. occupy the same place as before
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
        val res       = Group.play(target = target, addAction = addAction)
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