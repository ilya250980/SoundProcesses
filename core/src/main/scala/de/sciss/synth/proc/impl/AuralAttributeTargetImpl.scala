/*
 *  AuralAttributeTargetImpl.scala
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

import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{AudioBus, BusNodeSetter, DynamicUser, NodeRef, Resource, Synth, Sys, Txn}
import de.sciss.synth
import de.sciss.synth.Ops.stringToControl
import de.sciss.synth.proc.AuralAttribute.{Scalar, Stream, Value}
import de.sciss.synth.{ControlSet, SynthGraph}

import scala.concurrent.stm.{Ref, TMap}

final class AuralAttributeTargetImpl[S <: Sys[S]](target: NodeRef.Full[S], val key: String, targetBus: AudioBus)
  extends AuralAttribute.Target[S] with ObservableImpl[S, Value] {

  override def toString = s"AuralAttribute.Target($target, $key, $targetBus)"

  import TxnLike.peer
  import targetBus.{numChannels, server}

  private def ctlName   = graph.Attribute.controlName(key)

  private[this] val map       = TMap.empty[AuralAttribute[S], Connected]
  private[this] val stateRef  = Ref[State](Empty)

  def valueOption(implicit tx: S#Tx): Option[Value] = stateRef().valueOption

  private final class Connected(val value: Value, val users: List[DynamicUser], val resources: List[Resource])
    extends DynamicUser {

    def attach()(implicit tx: S#Tx): this.type = {
      if (users    .nonEmpty) target.addUser(this)
      if (resources.nonEmpty) resources.foreach(target.addResource)
      this
    }

    def add()(implicit tx: Txn): Unit = users.foreach(_.add())

    def remove()(implicit tx: Txn): Unit = {
      if (resources.nonEmpty) resources.foreach(target.removeResource)
      if (users.nonEmpty) {
        target.removeUser(this)
        users.foreach(_.remove())
      }
    }

    override def toString = s"Connected($value, $users)"
  }

  private sealed trait State {
    def put   (attr: AuralAttribute[S], value: Value)(implicit tx: S#Tx): State
    def remove(attr: AuralAttribute[S])(implicit tx: S#Tx): State

    def valueOption(implicit tx: S#Tx): Option[Value]
  }

  private final class AddRemoveEdge(edge: NodeRef.Edge) extends DynamicUser {
    def add   ()(implicit tx: Txn): Unit = server.addEdge   (edge)
    def remove()(implicit tx: Txn): Unit = server.removeEdge(edge)

    override def toString = s"AddRemoveEdge($edge)"
  }

  private final class AddRemoveVertex(vertex: NodeRef) extends DynamicUser {
    def add   ()(implicit tx: Txn): Unit = server.addVertex(vertex)
    def remove()(implicit tx: Txn): Unit = {
      server.removeVertex(vertex)
      // node.free()
    }

    override def toString = s"AddRemoveVertex($vertex)"
  }

  private def putSingleScalar(attr: AuralAttribute[S], value: Scalar)(implicit tx: S#Tx): State = {
    val ctlSet = value.toControl(ctlName, numChannels = numChannels)
    // target.node.set(ctlSet)
    target.addControl(ctlSet)
    val cc = new Connected(value, users = Nil, resources = Nil)
    map.put(attr, cc).foreach(_.dispose())
    cc.attach()
    new Single(attr, cc)
  }

  private def putSingleStream(attr: AuralAttribute[S], value: Stream)(implicit tx: S#Tx): State = {
    val edge      = NodeRef.Edge(value.source, target)
    val edgeUser  = new AddRemoveEdge(edge)
    val busUser   = BusNodeSetter.mapper(ctlName, value.bus, target.node)
    val cc        = new Connected(value, users = edgeUser :: busUser :: Nil, resources = Nil)
    map.put(attr, cc).foreach(_.dispose())
    cc.attach()
    new Single(attr, cc)
  }

  // ----

  private[this] object Empty extends State {
    def put(attr: AuralAttribute[S], value: Value)(implicit tx: S#Tx): State =
      value match {
        case sc: Scalar => putSingleScalar(attr, sc)
        case sc: Stream => putSingleStream(attr, sc)
      }

    def remove(attr: AuralAttribute[S])(implicit tx: S#Tx): State =
      this // throw new NoSuchElementException(attr.toString)

    def valueOption(implicit tx: S#Tx): Option[Value] = None

    override def toString = "Empty"
  }

  // ----

  private final class Single(attr1: AuralAttribute[S], con1: Connected) extends State {
    def valueOption(implicit tx: S#Tx): Option[Value] = {
      val value = con1.value match {
        case vs: Scalar => vs
        case vs: Stream =>
          // N.B. for a single stream, we do not cross-map to the
          // target-bus, but simply use the input bus!
          Stream(vs.source /* target */, vs.bus /* targetBus */)
      }
      Some(value)
    }

    def put(attr: AuralAttribute[S], value: Value)(implicit tx: S#Tx): State =
      if (attr == attr1) {
        Empty.put(attr, value)
      } else {
        con1.dispose()
        val tgtBusUser = BusNodeSetter.mapper(ctlName, targetBus, target.node)
        target.addUser(tgtBusUser)
        val c1 = mkVertex(con1.value)
        val c2 = mkVertex(     value)
        map.put(attr1, c1)
        map.put(attr , c2)
        new Multiple(tgtBusUser)
      }

    def remove(attr: AuralAttribute[S])(implicit tx: S#Tx): State = {
      // note: now that there is no general `add` method without
      // passing values, it is totally valid that an attribute
      // calls `remove` even if it hadn't called any `put` method
      // and thus there is no entry in the `map`.
      val opt = map.remove(attr) /* .fold(throw new NoSuchElementException(attr.toString)) (_.dispose()) */
      opt.fold[State](this) { value =>
        value.dispose()
        if (!con1.value.isScalar) {
          // We had a `mapan` for which the bus input is now gone.
          // Right now, `ButNodeSetter.mapper` does _not_ undo the
          // mapping if you remove it. XXX TODO -- I don't know if it should...
          // Therefore, to avoid glitches from stale bus contents,
          // we must explicitly set the control to some value (i.e. zero)
          val ctlSet: ControlSet =
            if (numChannels == 1) ctlName -> 0f
            else                  ctlName -> Vector.fill(numChannels)(0f)
          target.addControl(ctlSet)
        }
        Empty
      }
    }

    override def toString = s"Single($attr1, $con1)"
  }

  // ----

  private final class Multiple(tgtBusUser: DynamicUser) extends State {
    def valueOption(implicit tx: S#Tx): Option[Value] = Some(Stream(target, targetBus))

    def put(attr: AuralAttribute[S], value: Value)(implicit tx: S#Tx): State = {
      val con = mkVertex(value)
      map.put(attr, con).foreach(_.dispose())
      this
    }

    def remove(attr: AuralAttribute[S])(implicit tx: S#Tx): State = {
      val opt = map.remove(attr) // .fold(throw new NoSuchElementException(attr.toString))(_.dispose())
      opt.fold[State](this) { value =>
        value.dispose()
        map.size match {
          case 1 =>
            val (aa, cc) = map.head
            target.removeUser(tgtBusUser)
            tgtBusUser.dispose()  // XXX TODO --- not sure this should be done by `removeUser` automatically
            cc.dispose()
            Empty.put(aa, cc.value)
          case x =>
            assert(x > 1, s"map.size is $x")
            this
        }
      }
    }

    override def toString = "Multiple"
  }
  
  // ----

  private def mkVertex(value: Value)(implicit tx: S#Tx): Connected = {
    def make(syn: Synth, users0: List[DynamicUser]): Connected = {
      val vertexUser  = new AddRemoveVertex(syn)
      val outEdge     = NodeRef.Edge(syn, target)
      val outEdgeUser = new AddRemoveEdge(outEdge)
      val outBusUser  = BusNodeSetter.writer("out", targetBus, syn)
      val users       = vertexUser :: outEdgeUser :: outBusUser :: users0

      val cc = new Connected(value, users = users, resources = syn :: Nil)
      cc.attach()
    }

    value match {
      case sc: Scalar =>
        val g = SynthGraph {
          import synth._
          import ugen._
          val in = "in".kr(Vector.fill(numChannels)(0f))
          Out.ar("out".kr, in)
        }
        val values0     = sc.values
        val inChannels  = values0.length
        val syn         = Synth.play(g, nameHint = Some("attr-set"))(target = server,
          args = List("in" -> Vector.tabulate[Float](numChannels)(i => values0(i % inChannels))))
        make(syn, Nil)

      case sc: Stream =>
        // - basically the same synth (mapped control in, bus out)
        // - .reader/.mapper source-bus; .write targetBus
        // - add both vertices, add both edges
        val inChannels  = sc.bus.numChannels
        val g = SynthGraph {
          import synth._
          import ugen._
          val in  = "in".ar(Vector.fill(inChannels)(0f)) // In.ar("in".kr, inChannels)
          val ext = Vector.tabulate(numChannels)(in \ _)
          Out.ar("out".kr, ext)
        }
        val syn         = Synth.play(g, nameHint = Some("attr-map"))(
          target = server, dependencies = sc.source.node :: Nil)
        val inEdge      = NodeRef.Edge(sc.source, syn)
        val inEdgeUser  = new AddRemoveEdge(inEdge)
        val inBusUser   = BusNodeSetter.mapper("in" , sc.bus   , syn)
        val users0      = inEdgeUser :: inBusUser :: Nil
        make(syn, users0)
    }
  }

  def put(attr: AuralAttribute[S], value: Value)(implicit tx: S#Tx): Unit = {
    val oldState = stateRef()
    val newState = oldState.put(attr, value)
    updateState(oldState, newState)
  }

  def remove(attr: AuralAttribute[S])(implicit tx: S#Tx): Unit = {
    val oldState = stateRef()
    val newState = oldState.remove(attr)
    updateState(oldState, newState)
  }

  private def updateState(before: State, now: State)(implicit tx: S#Tx): Unit =
    if (before != now) {
      stateRef() = now
      now.valueOption.foreach(fire)
    }
}