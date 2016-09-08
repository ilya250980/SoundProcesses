/*
 *  SynthGraphs.scala
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

import java.util

import de.sciss.lucre.event.{Event, Dummy, EventLike, Targets}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Copy, Elem, Obj, Sys}
import de.sciss.lucre.expr
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.ugen.{Constant, ControlProxyLike}
import de.sciss.synth.{Lazy, MaybeRate, SynthGraph, proc}

import scala.annotation.switch
import scala.util.control.NonFatal

object SynthGraphObj extends expr.impl.ExprTypeImpl[SynthGraph, SynthGraphObj] {
  final val typeID = 16

  import proc.{SynthGraphObj => Repr}

  protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
    new _Const[S](id, value)

  protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)
                                  (implicit tx: S#Tx): Var[S] = {
    val res = new _Var[S](targets, vr)
    if (connect) res.connect()
    res
  }

  private final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
    extends ConstImpl[S] with Repr[S]

  private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
    extends VarImpl[S] with Repr[S]

  /** A serializer for synth graphs. */
  object valueSerializer extends ImmutableSerializer[SynthGraph] {
    private final val SER_VERSION = 0x5347

    // private final class RefMapOut {
    // var map   = Map.empty[Product, Int]
    // val map   = collection.mutable.Map.empty[Product, Int] // not faster than immutable map!
    // var count = 0
    // }

      // we use an identity hash map, because we do _not_
      // want to alias objects in the serialization; the input
      // is an in-memory object graph.
    private type RefMapOut = util.IdentityHashMap[Product, Integer]

    private final class RefMapIn {
      var map   = Map.empty[Int, Product]
      // val map   = collection.mutable.Map.empty[Int, Product]
      var count = 0
    }

    private def writeProduct(p: Product, out: DataOutput, ref: RefMapOut): Unit = {
      val id0Ref = ref.get(p)
      // val id0 = ref.map.getOrElse(p, -1)
      if (id0Ref != null) {
        out.writeByte('<')
        out.writeInt(id0Ref)
        return
      }
      out.writeByte('P')
      val pck     = p.getClass.getPackage.getName
      val prefix  = p.productPrefix
      val name    = if (pck == "de.sciss.synth.ugen") prefix else s"$pck.$prefix"
      out.writeUTF(name)
      out.writeShort(p.productArity)
      p.productIterator.foreach(writeElem(_, out, ref))

      val id     = ref.size() // count
      // ref.map   += ((p, id))
      // ref.count  = id + 1
      ref.put(p, id)
    }

    private def writeElemSeq(xs: Seq[Any], out: DataOutput, ref: RefMapOut): Unit = {
      out.writeByte('X')
      out.writeInt(xs.size)
      xs.foreach(writeElem(_, out, ref))
    }

    private def writeElem(e: Any, out: DataOutput, ref: RefMapOut): Unit =
      e match {
        case c: Constant =>
          out.writeByte('C')
          out.writeFloat(c.value)
        case r: MaybeRate =>
          out.writeByte('R')
          out.writeByte(r.id)
        case o: Option[_] =>
          out.writeByte('O')
          out.writeBoolean(o.isDefined)
          if (o.isDefined) writeElem(o.get, out, ref)
        case xs: Seq[_] =>  // 'X'. either indexed seq or var arg (e.g. wrapped array)
          writeElemSeq(xs, out, ref)
        case p: Product =>
          writeProduct(p, out, ref) // 'P' or '<'
        case i: Int =>
          out.writeByte('I')
          out.writeInt(i)
        case s: String =>
          out.writeByte('S')
          out.writeUTF(s)
        case b: Boolean   =>
          out.writeByte('B')
          out.writeBoolean(b)
        case f: Float =>
          out.writeByte('F')
          out.writeFloat(f)
        case d: Double =>
          out.writeByte('D')
          out.writeDouble(d)
      }

    def write(v: SynthGraph, out: DataOutput): Unit = {
      out.writeShort(SER_VERSION)
      writeNew(v, out)
    }

    private def writeNew(v: SynthGraph, out: DataOutput): Unit = {
      val ref = new RefMapOut
      writeElemSeq(v.sources, out, ref)
      val ctl = v.controlProxies
      out.writeByte('T')
      out.writeInt(ctl.size)
      ctl.foreach(writeProduct(_, out, ref))
    }

    // expects that 'X' byte has already been read
    private def readIdentifiedSeq(in: DataInput, ref: RefMapIn): Seq[Any] = {
      val num = in.readInt()
      Vector.fill(num)(readElem(in, ref))
    }

    // expects that 'P' byte has already been read
    private def readIdentifiedProduct(in: DataInput, ref: RefMapIn): Product = {
      val prefix    = in.readUTF()
      val arity     = in.readShort()
      val className = if (Character.isUpperCase(prefix.charAt(0))) s"de.sciss.synth.ugen.$prefix" else prefix

      val res = try {
        if (arity == 0 && className.charAt(className.length - 1) == '$') {
          // case object
          val companion = Class.forName(s"$className").getField("MODULE$").get(null)
          companion.asInstanceOf[Product]

        } else {

          // cf. stackoverflow #3039822
          val companion = Class.forName(s"$className$$").getField("MODULE$").get(null)
          val elems = new Array[AnyRef](arity)
          var i = 0
          while (i < arity) {
            elems(i) = readElem(in, ref).asInstanceOf[AnyRef]
            i += 1
          }
          //    val m         = companion.getClass.getMethods.find(_.getName == "apply")
          //      .getOrElse(sys.error(s"No apply method found on $companion"))
          val ms = companion.getClass.getMethods
          var m = null: java.lang.reflect.Method
          var j = 0
          while (m == null && j < ms.length) {
            val mj = ms(j)
            if (mj.getName == "apply" && mj.getParameterTypes.length == arity) m = mj
            j += 1
          }
          if (m == null) sys.error(s"No apply method found on $companion")

          m.invoke(companion, elems: _*).asInstanceOf[Product]
        }

      } catch {
        case NonFatal(e) =>
          throw new IllegalArgumentException(s"While de-serializing $prefix", e)
      }

      val id        = ref.count
      ref.map      += ((id, res))
      ref.count     = id + 1
      res
    }

    private def readElem(in: DataInput, ref: RefMapIn): Any = {
      (in.readByte(): @switch) match {
        case 'C' => Constant(in.readFloat())
        case 'R' => MaybeRate(in.readByte())
        case 'O' => if (in.readBoolean()) Some(readElem(in, ref)) else None
        case 'X' => readIdentifiedSeq(in, ref)
        case 'P' => readIdentifiedProduct(in, ref)
        case '<' =>
          val id = in.readInt()
          ref.map(id)
        case 'I' => in.readInt()
        case 'S' => in.readUTF()
        case 'B' => in.readBoolean()
        case 'F' => in.readFloat()
        case 'D' => in.readDouble()
      }
    }

    def read(in: DataInput): SynthGraph = {
      val cookie  = in.readShort()
      require(cookie == SER_VERSION, s"Unexpected cookie $cookie")
      val res2  = readNew(in)
      res2
    }

    private def readNew(in: DataInput): SynthGraph = {
      val ref     = new RefMapIn
      val b1 = in.readByte()
      require(b1 == 'X')    // expecting sequence
      val numSources  = in.readInt()
      val sources     = Vector.fill(numSources) {
        readElem(in, ref).asInstanceOf[Lazy]
      }
      val b2 = in.readByte()
      require(b2 == 'T')    // expecting set
      val numControls = in.readInt()
      val controls    = Set.newBuilder[ControlProxyLike] // stupid Set doesn't have `fill` and `tabulate` methods
      var i = 0
      while (i < numControls) {
        controls += readElem(in, ref).asInstanceOf[ControlProxyLike]
        i += 1
      }
      SynthGraph(sources, controls.result())
    }

    //  private def readOld(in: DataInput): SynthGraph = {
    //    val ois = new java.io.ObjectInputStream(in.asInputStream)
    //    val res = ois.readObject().asInstanceOf[SynthGraph]
    //    ois.close()
    //    res
    //  }
  }
  //  private val map = mutable.Map.empty[String, SynthGraph]
  //
  //  /** Adds a predefined synth graph which is serialized through a key.
  //    * Care must be taken so that the key is unique and that the graph
  //    * is registered before any possible de-serialization (which would
  //    * fail if no in-memory graph is found under the key)
  //    */
  //  def add(key: String, graph: SynthGraph): Unit =
  //    map.synchronized(map += (key, graph))

  // private final val oldTapeCookie = 1
  private final val emptyCookie   = 4
  private final val tapeCookie    = 5

  // def valueSerializer: ImmutableSerializer[SynthGraph] = ValueSerializer

//  def readValue (                   in : DataInput ): SynthGraph  = ValueSerializer.read (       in )
//  def writeValue(value: SynthGraph, out: DataOutput): Unit        = ValueSerializer.write(value, out)

  override protected def readCookie[S <: Sys[S]](in: DataInput, access: S#Acc, cookie: Byte)(implicit tx: S#Tx): Ex[S] =
    cookie match {
      case /* `oldTapeCookie` | */ `emptyCookie` | `tapeCookie` =>
        val id = tx.readID(in, access)
        new Predefined(id, cookie)
      case _ => super.readCookie(in, access, cookie)
      //      case `mapCookie`  =>
      //        val key     = in.readUTF()
      //        val graph   = map.synchronized(map(key))
      //        new MapImpl(key, graph)
    }

  // private final class MapImpl[S <: Sys[S]]

  //  private lazy val oldTapeSynthGraph: SynthGraph =
  //    SynthGraph {
  //      import de.sciss.synth._
  //      import ugen._
  //      val sig   = graph.scan.In(Proc.Obj.graphAudio)
  //      val bus   = graph.attribute(ObjKeys.attrBus   ).ir(0)
  //      val mute  = graph.attribute(ObjKeys.attrMute  ).ir(0)
  //      val env   = graph.FadeInOut(ObjKeys.attrFadeIn, ObjKeys.attrFadeOut).ar
  //      val amp   = env * (1 - mute)
  //      Out.ar(bus, sig * amp)
  //    }

  private lazy val tapeSynthGraph: SynthGraph =
    SynthGraph {
      import de.sciss.synth._
      val sig   = graph.VDiskIn  .ar(Proc.graphAudio)
      val gain  = graph.Attribute.kr(ObjKeys.attrGain, 1.0)
      val mute  = graph.Attribute.kr(ObjKeys.attrMute, 0.0)
      val env   = graph.FadeInOut.ar
      val amp   = env * ((1 - mute) * gain)
      val out   = sig * amp
      // (out \ 0).poll(label = "disk")
      graph.ScanOut(out)
    }

  private val emptySynthGraph = SynthGraph {}

  def tape   [S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(tapeCookie   )
  // def tapeOld[S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(oldTapeCookie)
  def empty  [S <: Sys[S]](implicit tx: S#Tx): Ex[S] = apply(emptyCookie  )

  private def apply[S <: Sys[S]](cookie: Int)(implicit tx: S#Tx): Ex[S] = {
    val id = tx.newID()
    new Predefined(id, cookie)
  }

  private final class Predefined[S <: Sys[S]](val id: S#ID, cookie: Int)
    extends SynthGraphObj[S] with Expr.Const[S, SynthGraph] {

    def event(slot: Int): Event[S, Any] = throw new UnsupportedOperationException

    def tpe: Obj.Type = SynthGraphObj

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new Predefined(txOut.newID(), cookie) // .connect()

    def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeID)
      out.writeByte(cookie)
      id.write(out)
    }

    def value(implicit tx: S#Tx): SynthGraph = constValue

    def changed: EventLike[S, Change[SynthGraph]] = Dummy[S, Change[SynthGraph]]

    def dispose()(implicit tx: S#Tx) = ()

    def constValue: SynthGraph = cookie match {
      // case `oldTapeCookie`  => oldTapeSynthGraph
      case `emptyCookie`    => emptySynthGraph
      case `tapeCookie`     => tapeSynthGraph
    }
  }
}
trait SynthGraphObj[S <: Sys[S]] extends Expr[S, SynthGraph]