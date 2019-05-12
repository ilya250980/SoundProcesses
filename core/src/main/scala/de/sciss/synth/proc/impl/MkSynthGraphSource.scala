/*
 *  MkSynthGraphSource.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import java.{util => ju}

import de.sciss.synth
import de.sciss.synth.proc.graph
import de.sciss.synth.ugen.{BinaryOpUGen, Constant, UnaryOpUGen}
import de.sciss.synth.{GE, Lazy, MaybeRate, Rate, SynthGraph, UGenSpec, UndefinedRate}

import scala.collection.immutable.{IndexedSeq => Vec}

object MkSynthGraphSource {
  private final case class ArgAssign(name: Option[String], shape: UGenSpec.SignalShape, value: Any)

  private sealed trait GraphLine {
    var uses    = Set   .empty[String]
    var valName = Option.empty[String]

    def elemName: String

    def hasArgNamed(n: String): Boolean
  }

  private final class StdGraphLine(val elemName: String, val constructor: String, val args: Vec[ArgAssign])
    extends GraphLine {

    def hasArgNamed(n: String): Boolean = {
      val nOpt = Some(n)
      args.exists(_.name == nOpt)
    }
  }

  private final class AttrGraphLine(val peer: graph.Attribute) extends GraphLine {
    def elemName: String = "Attr" // --- only used for val names; "Attribute"

    def hasArgNamed(n: String): Boolean = false
  }

  private[this] def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt) else String.valueOf(ch)
  }

  /** Escapes characters such as newlines and quotation marks in a string. */
  def escape(s: String): String = s.flatMap(escapedChar)
  /** Escapes characters such as newlines in a string, and adds quotation marks around it.
    * That is, formats the string as a string literal in a source code.
    */
  def quote (s: String): String = "\"" + escape(s) + "\""

  /** Creates source code for a given synth graph. */
  def apply(g: SynthGraph): String = {
    val ugenMap = UGenSpec.standardUGens

    var lines   = Vector.empty[GraphLine]
    val lazyMap = new ju.IdentityHashMap[Lazy, GraphLine]

    def mkLine(elem: Product): GraphLine = elem match {
      case attr @ graph.Attribute(_, _, _, -1)  => new AttrGraphLine(attr)
      case other                                => mkStdLine(other)
    }

    def mkStdLine(elem: Product): GraphLine = {
      val elemName  = elem.productPrefix
      val argVals   = elem.productIterator.toIndexedSeq

      val line = ugenMap.get(elemName).fold[GraphLine] {
        val ins = argVals.map(ArgAssign(None, UGenSpec.SignalShape.Generic, _))

        new StdGraphLine(elemName = elemName, constructor = "apply", args = ins)
      } { spec =>
        val (rate: MaybeRate, rateMethod: UGenSpec.RateMethod, argVals1: Vec[Any]) = spec.rates match {
          case UGenSpec.Rates.Implied(r, m) => (r, m, argVals)
          case UGenSpec.Rates.Set(_) =>
            argVals.head match {
              case r: Rate => (r, UGenSpec.RateMethod.Default, argVals.tail)
              // case x => throw new MatchError(s"For spec $spec, the first arg $x is not of type Rate")
              case _ =>
                // this currently happens for helper elements such as `LinLin`
                (UndefinedRate, UGenSpec.RateMethod.Default, argVals)
            }
        }
        val rateMethodName = rateMethod match {
          case UGenSpec.RateMethod.Alias (name) => name
          case UGenSpec.RateMethod.Custom(name) => name
          case UGenSpec.RateMethod.Default      => rate.toOption.fold("apply")(_.methodName)
        }
        val ins = (spec.args zip argVals1).map { case (arg, argVal) =>
          val shape = arg.tpe match {
            case UGenSpec.ArgumentType.GE(sh, _) => sh
            case _ => UGenSpec.SignalShape.Generic
          }
          ArgAssign(Some(arg.name), shape, argVal)
        }
        new StdGraphLine(elemName = elemName, constructor = rateMethodName, args = ins)
      }
      line
    }

    g.sources.zipWithIndex.foreach { case (elem, elemIdx) =>
      val line      = mkLine(elem)
      lines       :+= line
      lazyMap.put(elem, line)
      val elemName  = elem.productPrefix

      val args = line match {
        case std: StdGraphLine => std.args
        case _ => Vector.empty
      }

      args.foreach {
        case ArgAssign(argNameOpt, _, argVal: Lazy) =>
          val ref = lazyMap.get(argVal)
          if (ref == null) {
            val argValS = argVal.productPrefix
            val argS = argNameOpt.fold(argValS)(n => s"$n = $argValS")
            Console.err.println(s"Missing argument reference for $argS in $elemName in line $elemIdx")
          } else {
            val argName = argNameOpt.getOrElse("unnamed")
            ref.uses   += argName
          }

        case ArgAssign(argNameOpt, _, argVal: Product) if argVal.productPrefix == "GESeq" => // XXX TODO -- quite hackish
          val elems = argVal.productIterator.next().asInstanceOf[Vec[GE]]
          elems.foreach {
            case elem: Lazy =>
              val ref = lazyMap.get(elem)
              if (ref == null) {
                val argValS = elem.productPrefix
                val argS = argNameOpt.fold(argValS)(n => s"$n = $argValS")
                Console.err.println(s"Missing argument reference for $argS in $elemName seq in line $elemIdx")
              } else {
                ref.uses   += "unnamed"
              }

            case _ =>
          }

        case _ =>
      }
    }

    def uncapitalize(in: String): String = if (in.isEmpty) in else
      in.updated(0, Character.toLowerCase(in.charAt(0)))

    // assign preliminary val-names
    lines.foreach { line =>
      val uses = line.uses
      if (uses.nonEmpty) (uses - "unnamed").toList match {
        case single :: Nil if single != "unnamed" => line.valName = Some(single)
        case _ =>
          val nameUp0 = line match {
            case std: StdGraphLine if std.elemName == "BinaryOpUGen" =>
              val x = std.args.head.value.getClass.getName
              x.substring(0, x.length - 1)
            case other => other.elemName
          }

          val di      = nameUp0.lastIndexOf('$')
          val nameUp  = nameUp0.substring(di + 1)
          val nameLo  = uncapitalize(nameUp)
          line.valName = Some(nameLo)
      }
    }
    // make sure val-names are unique
    lines.zipWithIndex.foreach { case (line, li) =>
      line.valName.foreach { name0 =>
        val same = lines.filter(_.valName == line.valName)
        // cf. https://issues.scala-lang.org/browse/SI-9353
        val si9353 = lines.iterator.zipWithIndex.exists { case (line1, lj) =>
          lj < li && line.valName.exists(line1.hasArgNamed)
        }
        if (same.size > 1 || si9353) {
          same.zipWithIndex.foreach { case (line1, i) =>
            line1.valName = Some(s"${name0}_$i")
          }
        }
      }
    }
    // calc indentation
    val maxValNameSz0 = lines.foldLeft(0)((res, line) => line.valName.fold(res)(n => math.max(n.length, res)))
    val maxValNameSz  = maxValNameSz0 | 1 // odd

    def mkFloatString(f: Float): String =
      if (f.isPosInfinity) "inf" else if (f.isNegInfinity) "-inf" else if (f.isNaN) "Float.NaN" else s"${f}f"

    def mkFloatAsDoubleString(f: Float): String =
      if (f.isPosInfinity) "inf" else if (f.isNegInfinity) "-inf" else if (f.isNaN) "Double.NaN" else f.toString

    def mkDoubleString(d: Double): String =
      if (d.isPosInfinity) "inf" else if (d.isNegInfinity) "-inf" else if (d.isNaN) "Double.NaN" else d.toString

    def mkLineSource(line: GraphLine): String = {
      val invoke = line match {
        case std: StdGraphLine    => mkStdLineSource(std)
        case attr: AttrGraphLine  => mkAttrLineSource(attr)
      }
      line.valName.fold(invoke) { valName =>
        val pad = " " * (maxValNameSz - valName.length)
        s"val $valName$pad = $invoke"
      }
    }

    def mkAttrLineSource(line: AttrGraphLine): String = {
      import line.peer._
      val dfStr = default.fold("") { values =>
        val inner = if (values.size == 1) mkFloatAsDoubleString(values.head)
        else values.map(mkFloatString).mkString("Vector(", ", ", ")")
        s"($inner)"
      }
      s"${quote(key)}.${rate.methodName}$dfStr"
    }

    def mkStdLineSource(line: StdGraphLine): String = {
      val numArgs = line.args.size
      val args    = line.args.zipWithIndex.map { case (arg, ai) =>
        def mkString(x: Any): String = x match {
          case Constant(c) =>
            import UGenSpec.SignalShape._
            arg.shape match {
              case Int | Trigger | Gate | Switch if c == c.toInt => c.toInt.toString
              case DoneAction => synth.DoneAction(c.toInt).toString
              case _ => mkFloatAsDoubleString(c)
            }

          case l: Lazy =>
            val line1 = Option(lazyMap.get(l)).getOrElse(mkLine(l))
            line1.valName.getOrElse(mkLineSource(line1))

          case sq: Product if sq.productPrefix == "GESeq" =>
            val peer = sq.productIterator.next().asInstanceOf[Vec[GE]]
            peer.map(mkString).mkString("Seq[GE](", ", ", ")")

          case s: String =>
            val escaped = quote(s)
            escaped

          case f: Float   => mkFloatString (f)
          case d: Double  => mkDoubleString(d)

          case i: Int     => i.toString
          case n: Long    => s"${n}L"
          case b: Boolean => b.toString

          case opt: Option[_] =>
            opt.map(mkString).toString

          case v: Vec[_] =>
            val argsS = v.map(mkString)
            argsS.mkString("Vector(", ", ", ")")

          case r: Rate => r.toString

          case other =>
            s"$other /* could not parse! */"
        }
        val valString = mkString(arg.value)
        if (numArgs == 1) valString else arg.name.fold(valString) { argName =>
          if (ai == 0 && argName == "in") valString else s"$argName = $valString"
        }
      }
      val invoke = if (line.elemName == "BinaryOpUGen") {
        line.args.head.value match {
          case op: BinaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a, b) = args
            //            val a = if ((opS == "min" || opS == "max") && line.args(1).value.isInstanceOf[Constant])
            //              s"Constant(${a0}f)"
            //            else a0
            s"$a $opS $b"
        }
      } else if (line.elemName == "UnaryOpUGen") {
        line.args.head.value match {
          case op: UnaryOpUGen.Op =>
            val opS = uncapitalize(op.name)
            val Seq(_, a) = args
            s"$a.$opS"
        }
      } else {
        val cons      = if (line.constructor == "apply") "" else s".${line.constructor}"
        val elemName  = line.elemName.replace('$', '.')
        val select    = s"$elemName$cons"
        if (args.isEmpty && cons.nonEmpty) select else {
          // cheesy hack to somehow break the line at an arbitrary point so it doesn't get too long
          val sb  = new java.lang.StringBuilder(64)
          var len = select.length + 1
          sb.append(select)
          sb.append('(')
          var con = ""
          args.foreach { arg =>
            sb.append(con)
            len += con.length
            val aLen = arg.length
            if (len + aLen > 80) {
              sb.append("\n  ")
              len = 0 // we don't know how much the first line is indented, so other value makes no sense
            }
            sb.append(arg)
            len += aLen
            con = ", "
          }
          sb.append(')')
          sb.toString
        }
      }
      invoke
    }

    // turn to source
    val linesS = lines.map(mkLineSource)
    linesS.mkString("\n")
  }
}
