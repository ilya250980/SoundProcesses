/*
 *  SynthGraphs.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.util

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.{DummyEvent, ExprTypeImpl}
import de.sciss.lucre.{Copy, Elem, Event, EventLike, Expr, Ident, Obj, Txn, Var => LVar}
import de.sciss.model.Change
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.ugen.{Constant, ControlProxyLike}
import de.sciss.synth.{Lazy, MaybeRate, SynthGraph, proc}

import scala.annotation.{switch, tailrec}
import scala.util.control.NonFatal

object SynthGraphObj extends ExprTypeImpl[SynthGraph, SynthGraphObj] {
  final val typeId = 16

  import proc.{SynthGraphObj => Repr}

  def tryParse(value: Any): Option[SynthGraph] = value match {
    case x: SynthGraph  => Some(x)
    case _              => None
  }

  protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
    new _Const[T](id, value)

  protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                  (implicit tx: T): Var[T] = {
    val res = new _Var[T](targets, vr)
    if (connect) res.connect()
    res
  }

  private final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
    extends ConstImpl[T] with Repr[T]

  private final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
    extends VarImpl[T] with Repr[T]

  /** A format for synth graphs. */
  object valueFormat extends ConstFormat[SynthGraph] {
    private final val SER_VERSION = 0x5347

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

    @tailrec
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
        case y: SynthGraph =>
          out.writeByte('Y')
          writeIdentifiedGraph(y, out, ref)
        // important: `Product` must come after all other types that might _also_ be a `Product`
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
      writeIdentifiedGraph(v, out, new RefMapOut)
    }

    private def writeIdentifiedGraph(v: SynthGraph, out: DataOutput, ref: RefMapOut): Unit = {
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
          val companion = Class.forName(className).getField("MODULE$").get(null)
          companion.asInstanceOf[Product]

        } else {

          // cf. stackoverflow #3039822
          val className1 = className + "$"
          val companion = Class.forName(className1).getField("MODULE$").get(null)
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

    // taken: < B C D F I O P R S X Y
    private def readElem(in: DataInput, ref: RefMapIn): Any = {
      (in.readByte(): @switch) match {
        case 'C' => Constant(in.readFloat())
        case 'R' => MaybeRate(in.readByte())
        case 'O' => if (in.readBoolean()) Some(readElem(in, ref)) else None
        case 'X' => readIdentifiedSeq    (in, ref)
        case 'Y' => readIdentifiedGraph  (in, ref)
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
      val res2  = readIdentifiedGraph(in, new RefMapIn)
      res2
    }

    private def readIdentifiedGraph(in: DataInput, ref: RefMapIn): SynthGraph = {
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
  }

  // private final val oldTapeCookie = 1
  private final val emptyCookie   = 4
  private final val tapeCookie    = 5

  override protected def readCookie[T <: Txn[T]](in: DataInput, cookie: Byte)(implicit tx: T): E[T] =
    cookie match {
      case /* `oldTapeCookie` | */ `emptyCookie` | `tapeCookie` =>
        val id = tx.readId(in)
        new Predefined(id, cookie)
      case _ => super.readCookie(in, cookie)
    }

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

  private val tapeSynthGraphSource =
    """val sig   = VDiskIn.ar("sig")
      |val gain  = "gain".kr(1.0)
      |val mute  = "mute".kr(0)
      |val env   = FadeInOut.ar
      |val amp   = env * ((1 - mute) * gain)
      |val out   = sig * amp
      |// (out \ 0).poll(label = "disk")
      |ScanOut(out)
      |""".stripMargin

  private val emptySynthGraph = SynthGraph {}

  def tape   [T <: Txn[T]](implicit tx: T): E[T] = apply(tapeCookie   )
  // def tapeOld[T <: Txn[T]](implicit tx: T): Ex[T] = apply(oldTapeCookie)
  def empty  [T <: Txn[T]](implicit tx: T): E[T] = apply(emptyCookie  )

  def tapeSource[T <: Txn[T]](implicit tx: T): Code.Obj[T] = {
    val v     = Code.SynthGraph(tapeSynthGraphSource)
    val res   = Code.Obj.newVar[T](v)
    res.name  = "tape"
    res
  }

  private def apply[T <: Txn[T]](cookie: Int)(implicit tx: T): E[T] = {
    val id = tx.newId()
    new Predefined(id, cookie)
  }

  private final class Predefined[T <: Txn[T]](val id: Ident[T], cookie: Int)
    extends SynthGraphObj[T] with Expr.Const[T, SynthGraph] {

    def event(slot: Int): Event[T, Any] = throw new UnsupportedOperationException

    def tpe: Obj.Type = SynthGraphObj

    def copy[Out <: Txn[Out]]()(implicit tx: T, txOut: Out, context: Copy[T, Out]): Elem[Out] =
      new Predefined(txOut.newId(), cookie) // .connect()

    def write(out: DataOutput): Unit = {
      out.writeInt(tpe.typeId)
      out.writeByte(cookie)
      id.write(out)
    }

    def value(implicit tx: T): SynthGraph = constValue

    def changed: EventLike[T, Change[SynthGraph]] = DummyEvent[T, Change[SynthGraph]]

    def dispose()(implicit tx: T): Unit = ()

    def constValue: SynthGraph = cookie match {
      // case `oldTapeCookie`  => oldTapeSynthGraph
      case `emptyCookie`    => emptySynthGraph
      case `tapeCookie`     => tapeSynthGraph
    }
  }
}
trait SynthGraphObj[T <: Txn[T]] extends Expr[T, SynthGraph]