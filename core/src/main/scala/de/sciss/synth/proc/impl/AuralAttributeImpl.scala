/*
 *  AuralAttributeImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.expr.{BooleanObj, DoubleObj, Expr, IntObj}
import de.sciss.lucre.stm.{TxnLike, Disposable, Obj}
import de.sciss.lucre.synth.{Txn, NodeRef, AudioBus, Sys}
import de.sciss.lucre.{expr, stm}
import de.sciss.synth.Curve
import de.sciss.synth.proc.AuralAttribute.{Instance, Target, Factory}
import de.sciss.synth.proc.AuralContext.AuxAdded

import scala.concurrent.stm.Ref

object AuralAttributeImpl {
  private[this] val sync = new AnyRef

  import TxnLike.peer

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](value: Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {
    val tid     = value.tpe.typeID
    val factory = map.getOrElse(tid, throw new IllegalArgumentException(s"No AuralAttribute available for $value"))
    factory(value.asInstanceOf[factory.Repr[S]])
  }

  private[this] var map = Map[Int, Factory](
    IntObj              .typeID -> IntAttribute,
    DoubleObj           .typeID -> DoubleAttribute,
    BooleanObj          .typeID -> BooleanAttribute,
    FadeSpec.Obj        .typeID -> FadeSpecAttribute,
//    DoubleVector        .typeID -> DoubleVectorAttribute,
//    Grapheme.Expr.Audio .typeID -> AudioGraphemeAttribute,
    Output              .typeID -> OutputAttribute,
    Folder              .typeID -> FolderAttribute
//    Timeline            .typeID -> ...
  )

  // private[this] final class PlayRef[S <: Sys[S]](val target: Target)

  // private[this] type PlayRef[S <: Sys[S]] = Target

  private[this] trait ExprImpl[S <: Sys[S], A] extends AuralAttribute[S] { attr =>
    // ---- abstract ----

    protected def exprH: stm.Source[S#Tx, Expr[S, A]]

    protected def mkValue(in: A): AuralAttribute.Value

    // ---- impl ----

    private[this] final class PlayRef(val target: Target)
      extends Instance {

      def dispose()(implicit tx: Txn): Unit = {
        playRef.transform(_.filterNot(_ == this))
        target.remove(this)
      }
    }

    private[this] var obs: Disposable[S#Tx] = _
    private[this] val playRef = Ref(List.empty[PlayRef])

    def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit /* Instance */ = {
      val p = new PlayRef(target)
      playRef.transform(p :: _)
      // require(playRef.swap(Some(p))(tx.peer).isEmpty)
      target.add(p)
      update(p, exprH().value)
      // p
    }

    private[this] def update(p: PlayRef, value: A)(implicit tx: S#Tx): Unit = {
      import p.target
      val ctlVal = mkValue(value)
      target.put(p, ctlVal)
    }

    def init(expr: Expr[S, A])(implicit tx: S#Tx): this.type = {
      obs = expr.changed.react { implicit tx => change =>
        playRef().foreach(update(_, change.now))
      }
      this
    }

//    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      playRef().foreach(_.dispose())
    }
  }

  private[this] trait NumberImpl[S <: Sys[S], A] extends ExprImpl[S, A] {
    final def preferredNumChannels(implicit tx: S#Tx): Int = 1
  }
  
  // ------------------- IntObj ------------------- 

  private[this] object IntAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = IntObj[S]

    def typeID = IntObj.typeID

    def apply[S <: Sys[S]](value: IntObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new IntAttribute(tx.newHandle(value)).init(value)
  }
  private[this] final class IntAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, IntObj[S]])
    extends NumberImpl[S, Int] {

    protected def mkValue(value: Int): AuralAttribute.Value = value.toFloat
  }

  // ------------------- DoubleObj ------------------- 

  private[this] object DoubleAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = DoubleObj[S]

    def typeID = DoubleObj.typeID

    def apply[S <: Sys[S]](value: DoubleObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new DoubleAttribute(tx.newHandle(value)).init(value)
  }
  private[this] final class DoubleAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, DoubleObj[S]])
    extends NumberImpl[S, Double] {

    protected def mkValue(value: Double): AuralAttribute.Value = value.toFloat
  }

  // ------------------- BooleanObj ------------------- 

  private[this] object BooleanAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = BooleanObj[S]

    def typeID = BooleanObj.typeID

    def apply[S <: Sys[S]](value: BooleanObj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new BooleanAttribute(tx.newHandle(value)).init(value)
  }
  private[this] final class BooleanAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, BooleanObj[S]])
    extends NumberImpl[S, Boolean] {

    protected def mkValue(value: Boolean): AuralAttribute.Value = if (value) 1f else 0f
  }

  // ------------------- FadeSpec.Obj ------------------- 

  private[this] object FadeSpecAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = FadeSpec.Obj[S]

    def typeID = FadeSpec.Obj.typeID

    def apply[S <: Sys[S]](value: FadeSpec.Obj[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new FadeSpecAttribute(tx.newHandle(value)).init(value)
  }
  private[this] final class FadeSpecAttribute[S <: Sys[S]](protected val exprH: stm.Source[S#Tx, FadeSpec.Obj[S]])
    extends ExprImpl[S, FadeSpec] {

    def preferredNumChannels(implicit tx: S#Tx): Int = 4

    protected def mkValue(spec: FadeSpec): AuralAttribute.Value = Vector[Float](
      (spec.numFrames / Timeline.SampleRate).toFloat, spec.curve.id.toFloat, spec.curve match {
        case Curve.parametric(c)  => c
        case _                    => 0f
      }, spec.floor
    )
  }

  // ------------------- Output ------------------- 

  private[this] object OutputAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = Output[S]

    def typeID = Output.typeID

    def apply[S <: Sys[S]](value: Output[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
      new OutputAttribute().init(value)
  }
  private[this] final class OutputAttribute[S <: Sys[S]]()(implicit context: AuralContext[S])
    extends AuralAttribute[S] { attr =>

    private[this] final class PlayRef(val target: Target)
      extends Instance {

      def dispose()(implicit tx: Txn): Unit = {
        playRef.transform(_.filterNot(_ == this))
        target.remove(this)
      }
    }

    private[this] val auralRef  = Ref(Option.empty[AuralOutput[S]])
    private[this] var obs: Disposable[S#Tx] = _
    private[this] val playRef   = Ref(List.empty[PlayRef])
    private[this] val aObsRef   = Ref(Option.empty[Disposable[S#Tx]])

    def preferredNumChannels(implicit tx: S#Tx): Int =
      auralRef().fold(-1)(_.bus.numChannels)

    def init(output: Output[S])(implicit tx: S#Tx): this.type = {
      val id  = output.id // idH()
      obs = context.observeAux[AuralOutput[S]](id) { implicit tx => {
        case AuxAdded(_, auralOutput) => auralSeen(auralOutput)
      }}
      context.getAux[AuralOutput[S]](id).foreach(auralSeen)
      this
    }

    private[this] def auralSeen(auralOutput: AuralOutput[S])(implicit tx: S#Tx): Unit = {
      auralRef() = Some(auralOutput)
      val aObs = auralOutput.react { implicit tx => {
        case AuralOutput.Play(n) =>
          playRef().foreach(update(_, auralOutput))
        case AuralOutput.Stop => // XXX TODO: ignore?
      }}
      aObsRef.swap(Some(aObs)).foreach(_.dispose())
      playRef().foreach(update(_, auralOutput))
    }

    def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit /* Instance */ = {
      val p = new PlayRef(target)
      playRef.transform(p :: _)
      target.add(p)
      auralRef().foreach(update(p, _))
      // p
    }

    private[this] def update(p: PlayRef, audioOutput: AuralOutput[S])(implicit tx: S#Tx): Unit =
      audioOutput.data.nodeOption.foreach(update1(p, _, audioOutput.bus))

    private[this] def update1(p: PlayRef, nodeRef: NodeRef, bus: AudioBus)(implicit tx: S#Tx): Unit = {
      import p.target
      target.put(p, AuralAttribute.Stream(nodeRef, bus))
    }

//    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      auralRef.set(None)
      aObsRef.swap(None).foreach(_.dispose())
      obs.dispose()
      playRef().foreach(_.dispose())
    }
  }

  // ------------------- Folder ------------------- 

  private[this] object FolderAttribute extends Factory {
    type Repr[S <: stm.Sys[S]] = Folder[S]

    def typeID = Folder.typeID

    def apply[S <: Sys[S]](value: Folder[S])
                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] = {

      val elemViews = value.iterator.map { elem =>
        AuralAttribute(elem)
      } .toVector

      new FolderAttribute(Ref(elemViews)).init(value)
    }
  }
  private[this] final class FolderAttribute[S <: Sys[S]](childAttrRef: Ref[Vector[AuralAttribute[S]]])
                                                        (implicit context: AuralContext[S])
    extends AuralAttribute[S] { attr =>

    import context.{scheduler => sched}

    private[this] final class PlayTime(val wallClock: Long,
                                       val timeRef: TimeRef.Apply, val target: Target)
      extends Instance {

      def shiftTo(newWallClock: Long): TimeRef.Apply = timeRef.shift(newWallClock - wallClock)

      def dispose()(implicit tx: Txn): Unit = {
        playRef.transform(_.filterNot(_ == this))
        target.remove(this)
        // childViews.swap(Vector.empty).foreach(_.dispose())
      }

//      def addChild(child: AuralAttribute[S])(implicit tx: S#Tx): Unit = {
//        val tForce    = shiftTo(sched.time)
//        val childView = child.play(tForce, target)
//        // childViews.transform(_ :+ childView)
//      }
    }

    private[this] val playRef = Ref(List.empty[PlayTime])
    private[this] var obs: Disposable[S#Tx] = _

    def preferredNumChannels(implicit tx: S#Tx): Int = {
      def loop(views: Vector[AuralAttribute[S]], res: Int): Int = views match {
        case head +: tail =>
          val ch = head.preferredNumChannels
          if (ch == -1) ch else loop(tail, math.max(res, ch))
        case _ => res
      }

      loop(childAttrRef(), -1)
    }

    def init(folder: Folder[S])(implicit tx: S#Tx): this.type = {
      // views.foreach(_.init())
      obs = folder.changed.react { implicit tx => upd => upd.changes.foreach {
        case expr.List.Added  (idx, child) =>
          val childAttr = AuralAttribute(child)
          childAttrRef.transform(_.patch(idx, childAttr :: Nil, 0))
          playRef().foreach { p =>
            // p.addChild(childAttr)
            val tForce    = p.shiftTo(sched.time)
            childAttr.play(tForce, p.target)
          }

        case expr.List.Removed(idx, child) =>
          childAttrRef.transform { in =>
            val childAttr = in(idx)
            childAttr.dispose()
            in.patch(idx, Nil, 1)
          }
      }}
      this
    }

    def play(timeRef: TimeRef, target: Target)(implicit tx: S#Tx): Unit /* Instance */ = {
      val tForce  = timeRef.force
      // require(playRef.swap(Some(p)).isEmpty)
      val childAttrs  = childAttrRef()
     childAttrs.foreach { childAttr =>
        childAttr.play(tForce, target)
      }
      val p = new PlayTime(sched.time, tForce, target /* , Ref(childViews) */)
      playRef.transform(p :: _)
      target.add(p)
      // p
    }

//    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
//      val views = elemViewsRef()
//      views.foreach(_.prepare(timeRef))
//    }

    def dispose()(implicit tx: S#Tx): Unit = {
      obs.dispose()
      playRef.swap(Nil).foreach(_.dispose())
      val views = childAttrRef()
      views.foreach(_.dispose())
    }
  }

  // ------------------- AudioGrapheme ------------------- 

//  private[this] object AudioGraphemeAttribute extends Factory {
//    type Repr[S <: stm.Sys[S]] = Grapheme.Expr.Audio[S]
//
//    def typeID = Grapheme.Expr.Audio.typeID
//
//    def apply[S <: Sys[S]](value: Grapheme.Expr.Audio[S])
//                          (implicit tx: S#Tx, context: AuralContext[S]): AuralAttribute[S] =
//      new AudioGraphemeAttribute().init(value)
//  }
//  private[this] final class AudioGraphemeAttribute[S <: Sys[S]]()
//                                                               (implicit context: AuralContext[S])
//    extends AuralAttribute[S] {
//
//    def preferredNumChannels(implicit tx: S#Tx): Int = audioH().value.numChannels
//
//    def init(audio: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): this.type = {
//      ...
//    }
//
//    def play(timeRef: TimeRef, builder: AuralAttributeTarget, numChannels: Int)(implicit tx: S#Tx): Unit = {
//      val ctlName   = graph.Attribute.controlName(key)
//        val audioVal  = a.value
//        val spec      = audioVal.spec
//        if (spec.numFrames != 1) {
//          sys.error(s"Audio grapheme $a must have exactly 1 frame to be used as scalar attribute")
//          // Console.err.println(s"Audio grapheme $a must have exactly 1 frame to be used as scalar attribute")
//          // throw MissingIn(AttributeKey(key))
//        }
//        val numCh = spec.numChannels // numChL.toInt
//        if (numCh > 4096) sys.error(s"Audio grapheme size ($numCh) must be <= 4096 to be used as scalar attribute")
//        chanCheck(numCh)
//        val bus = Bus.control(server, numCh)
//        val res = BusNodeSetter.mapper(ctlName, bus, b.node)
//        b.addUser(res)
//        val w = AudioArtifactScalarWriter(bus, audioVal)
//        b.addResource(w)
//      ...
//    }
//
//    def prepare(timeRef: TimeRef)(implicit tx: S#Tx): Unit = ...
//
//    def dispose()(implicit tx: S#Tx): Unit = {
//      ...
//    }
//  }
}