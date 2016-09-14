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
import de.sciss.lucre.synth.{Bus, DynamicUser, Group, Node, Resource, Server, Synth, Sys, Txn}
import de.sciss.synth.{ControlSet, NestedUGenGraphBuilder, addBefore, addToHead, audio, control}

import scala.concurrent.stm.Ref

object AuralNodeImpl {
  def apply[S <: Sys[S]](timeRef: TimeRef, wallClock: Long, node: Node)(implicit tx: Txn): AuralNode.Builder[S] = {
    // XXX TODO -- probably we can throw `users` and `resources` together as disposables
    val res = new Impl[S](timeRef, wallClock, node)
    node.server.addVertex(res)
    res
  }

  private final case class Result(tree: Tree, controls: List[ControlSet], buses: List[Bus])

  private sealed trait Tree { def main: Node }
  private final case class Leaf  (main: Synth)                                   extends Tree
  private final case class Branch(main: Group, syn: Synth, children: List[Tree]) extends Tree

  private def prepare(res0: NestedUGenGraphBuilder.Result, s: Server, nameHint: Option[String])
                     (implicit tx: Txn): Result = {
//    var defs  = List.empty[SynthDef]
//    var defSz = 0   // used to create unique def names
//    var msgs  = List.empty[osc.Message] // synchronous
    var ctl   = List.empty[ControlSet]
    var buses = List.empty[Bus]

    def loop(tree: NestedUGenGraphBuilder.Result): Tree = {
//      val name        = s"test-$defSz"
//      val sd          = SynthDef(name, tree.graph)
//      defs          ::= sd
//      defSz          += 1
      val syn         = Synth.expanded(s, tree.graph, nameHint = nameHint)
      val hasChildren = tree.children.nonEmpty

      val child: Tree = if (hasChildren) {
        val group = Group(s)
        val children = tree.children.map { cc =>
          val ccn = loop(cc)
          if (cc.id >= 0) ctl ::= NestedUGenGraphBuilder.pauseNodeCtlName(cc.id) -> ccn.main.peer.id
          ccn
        }
        Branch(main = group, syn = syn, children = children)

      } else {
        Leaf(syn)
      }

//      msgs ::= syn.newMsg(name, target = group, addAction = if (hasChildren) addToHead else addAction)

      tree.links.foreach { link =>
        val bus = link.rate match {
          case `audio`    => Bus.audio  (s, numChannels = link.numChannels)
          case `control`  => Bus.control(s, numChannels = link.numChannels)
          case other      => throw new IllegalArgumentException(s"Unsupported link rate $other")
        }
        buses ::= bus
        ctl   ::= NestedUGenGraphBuilder.linkCtlName(link.id) -> (??? : Int) // NNN bus.index
      }

      child
    }

    val top = loop(res0)
    Result(tree = top, controls = ctl, buses = buses)
//    mainNode.onEnd {
//      buses.foreach(_.free())
//    }
//
//    msgs ::= mainNode.setMsg(ctl.reverse: _*)
//    msgs ::= synth.message.SynthDefFree(defs.map(_.name): _*)
//
//    val b1 = osc.Bundle.now(msgs.reverse: _*)
//    val defL :: defI = defs
//    val async = defL.recvMsg(b1) :: defI.map(_.recvMsg)
//    val b2 = osc.Bundle.now(async.reverse: _*)
//
//    (b2, mainNode)
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