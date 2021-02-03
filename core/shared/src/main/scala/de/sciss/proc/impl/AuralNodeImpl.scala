/*
 *  AuralNodeImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.synth.{Bus, BusNodeSetter, DynamicUser, Group, Node, RT, Resource, Server, Synth, Txn}
import de.sciss.proc.{AuralNode, TimeRef}
import de.sciss.synth.{AddAction, ControlSet, NestedUGenGraphBuilder, addBefore, addToHead, addToTail, audio, control}

import scala.collection.immutable.{Seq => ISeq}
import scala.concurrent.stm.Ref

object AuralNodeImpl {
  def apply[T <: Txn[T]](timeRef: TimeRef, wallClock: Long, ubRes: NestedUGenGraphBuilder.Result,
                         server: Server, nameHint: Option[String])
                        (implicit tx: RT): AuralNode.Builder[T] = {
    val res = prepare(ubRes, server, nameHint = nameHint)
    // XXX TODO -- probably we can throw `users` and `resources` together as disposables
    val aNode = new Impl[T](timeRef, wallClock, tree = res.tree, setMap = res.controls, users0 = res.buses)
    server.addVertex(aNode)
    aNode
  }

  // ---- transformation of NestedUGenGraphBuilder.Result into a lucre-based tree ----

  private final case class Result(tree: Tree, controls: List[ControlSet], buses: List[BusNodeSetter])

  private sealed trait Tree {
    def synth: Synth

    def main: Node
    def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
            (implicit tx: RT): Unit
  }

  private final case class Leaf(synth: Synth) extends Tree {
    def main: Node = synth
    def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
            (implicit tx: RT): Unit =
      synth.play(target = target, args = args, addAction = addAction, dependencies = dependencies)
  }

  private final case class Branch(group: Group, synth: Synth, children: List[Tree]) extends Tree {
    def main: Node = group
    def play(target: Node, args: ISeq[ControlSet], addAction: AddAction, dependencies: List[Resource])
            (implicit tx: RT): Unit = {
      group.play(target = target, addAction = addAction, args = Nil, dependencies = dependencies)
      synth  .play(target = group , addAction = addToHead, args = Nil, dependencies = Nil         )
      children.foreach { ch =>
        ch .play(target = group , addAction = addToTail, args = Nil, dependencies = Nil         )
      }
      group.set(args: _*)
    }
  }

  private def prepare(res0: NestedUGenGraphBuilder.Result, s: Server, nameHint: Option[String])
                     (implicit tx: RT): Result = {
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
        Branch(group = group, synth = syn, children = children)

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
  private final class Impl[T <: Txn[T]](val timeRef: TimeRef, wallClock: Long, tree: Tree,
                                        private[this] var setMap: List[ControlSet], users0: List[DynamicUser])
    extends AuralNode.Builder[T] {

    import de.sciss.lucre.Txn.peer
    import tree.{main => graphMain}

    private[this] val users       = Ref(users0)
    private[this] val resources   = Ref(List.empty[Resource])

    private[this] val groupsRef   = Ref[Option[AllGroups]](graphMain match {
      case g: Group => Some(AllGroups(g))
      case _        => None
    })

    override def toString = s"AuralProc($graphMain)"

    def server: Server = graphMain.server

    def groupOption(implicit tx: RT): Option[Group] = groupsRef().map(_.main)

    def node(implicit tx: RT): Node = groupOption.getOrElse(graphMain)

    def synth: Synth = tree.synth

    def play()(implicit tx: T): Unit = {
      // `play` calls `requireOffline`, so we are safe against accidental repeated calls
      val target = server.defaultGroup
      tree.play(target = target, addAction = addToHead, args = setMap.reverse, dependencies = resources().reverse)
      users().reverse.foreach(_.add())
    }

    def shiftTo(newWallClock: Long): TimeRef = {
      val delta = newWallClock - wallClock
      timeRef.shift(delta)
    }

    def group()(implicit tx: T): Group =
      groupOption.getOrElse {
        val res = Group.play(graphMain, addBefore) // i.e. occupy the same place as before
        group_=(res)
        res
      }

    def group_=(newGroup: Group)(implicit tx: T): Unit =
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
    private[this] def preGroupOption(implicit tx: T): Option[Group] =
      groupsRef.get(tx.peer).flatMap(_.pre)

    def preGroup()(implicit tx: T): Group =
      preGroupOption.getOrElse {
        /* val main = */ group() // creates group if necessary
        val all       = groupsRef().get
        val target    = anchorNode()
        val addAction = addBefore
        val res       = Group.play(target = target, addAction = addAction)
        groupsRef.set(Some(all.copy(pre = Some(res))))
        res
      }

    private def anchorNode()(implicit tx: T): Node =
      groupsRef().flatMap(_.core) getOrElse graphMain

    private def moveAllTo(all: AllGroups, newGroup: Group)(implicit tx: T): Unit = {
      val core = anchorNode()
      core.moveToTail(newGroup)
      all.pre .foreach(_.moveBefore(core))
      all.post.foreach(_.moveAfter (core))

      all.back.foreach { g =>
        if (g.isOnline) g.moveToHead(newGroup)
      }
    }

    def dispose()(implicit tx: T): Unit = {
      node.free()
      users      .swap(Nil).reverse.foreach(_.dispose())
      resources  .swap(Nil).reverse.foreach(_.dispose())
      // disposables.swap(Nil).reverse.foreach(_.dispose())
      server.removeVertex(this)
    }

    def addControl(pair: ControlSet)(implicit tx: T): Unit = {
      if (graphMain.isOnline) node.set(pair)
      else setMap ::= pair
    }

    def addUser(user: DynamicUser)(implicit tx: RT): Unit = {
      users.transform(user :: _)
      if (graphMain.isOnline) user.add()
    }

    def removeUser(user: DynamicUser )(implicit tx: RT): Unit =
      users.transform(_.filterNot(_ == user))

    def addResource(resource: Resource)(implicit tx: RT): Unit =
      resources.transform(resource :: _)

    def removeResource(resource: Resource)(implicit tx: RT): Unit =
      resources.transform(_.filterNot(_ == resource))

//    def addDisposable(d: Disposable[T])(implicit tx: T): Unit =
//      disposables.transform(d :: _)
  }
}