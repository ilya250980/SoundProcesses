/*
 *  Macros.scala
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
package impl

import de.sciss.lucre
import de.sciss.lucre.Txn
import de.sciss.synth.SynthGraph

import scala.reflect.macros.blackbox

object Macros {
  def mkSource(c: blackbox.Context)(name: String, tree: c.Tree): String = {
    val pos = tree.pos
    val fullSource = pos.source.content
    val res = if (pos.isRange) new String(fullSource.slice(pos.start, pos.start + pos.end - pos.start)) else {
      // c.error(pos, s"Could not extract source")
      c.warning(pos, s"Could not extract $name source ($pos)")
      "// could not extract source"
    }
    indentSource(res)
  }

  def indentSource(in: String): String = {
    val inT     = in.trim
    val source0 = if (inT.startsWith("{") && inT.endsWith("}")) inT.substring(1, inT.length - 1) else in
    val res = {
      val arr0 = source0.split("\n")
      // println(s"----\n${arr0.head}\n----\n${arr0.last}\n----")
      val arr1 = arr0.dropWhile(_.trim.isEmpty)
      val arr  = arr1.reverse.dropWhile(_.trim.isEmpty).reverse
      if (arr.isEmpty) source0 else {
        val arrM = arr.iterator.filterNot(_.trim.isEmpty).map(_.prefixLength(_ == ' '))
        val arrI = if (arrM.isEmpty) arr else {
          val drop = arrM.min
          arr.map(ln => ln.substring(math.min(ln.length, drop)))
        }
        arrI.mkString("\n")
      }
    }
    res
  }

  // ---- Proc ----

  def procGraphWithSource[T <: Txn[T]](c: blackbox.Context)(body: c.Expr[Unit])(tx: c.Expr[T])
                                      /* note: implicits _are_ used! */ (implicit tt: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("proc", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.ProcMacroOps[T]]
      implicit val txc = tx.splice // don't annotate the type with `T`, it will break scalac
      val p                   = ext.`this`
      p.graph()               = SynthGraphObj.newConst[T](SynthGraph(body.splice))
      val code                = Code.SynthGraph(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[T](Code.Obj.newConst[T](code))
      p.attr.put(Proc.attrSource, codeObj)
      ()
    }
  }

  // ---- Control ----

  def controlGraphWithSource[T <: Txn[T]](c: blackbox.Context)(body: c.Expr[Unit])(tx: c.Expr[T])
                             /* note: implicits _are_ used! */(implicit tt: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("control", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.ControlMacroOps[T]]
      implicit val txc = tx.splice // don't annotate the type with `T`, it will break scalac
      val w                   = ext.`this`
      w.graph()               = Control.GraphObj.newConst[T](Control.Graph(body.splice))
      val code                = Code.Control(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[T](Code.Obj.newConst[T](code))
      w.attr.put(Control.attrSource, codeObj)
      ()
    }
  }

  // ---- Action ----

  def actionGraphWithSource[T <: Txn[T]](c: blackbox.Context)(body: c.Expr[lucre.expr.graph.Act])(tx: c.Expr[T])
                                        /* note: implicits _are_ used! */(implicit tt: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("action", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.ActionMacroOps[T]]
      implicit val txc = tx.splice // don't annotate the type with `T`, it will break scalac
      val w                   = ext.`this`
      w.graph()               = Action.GraphObj.newConst[T](Action.Graph(body.splice))
      val code                = Code.Action(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[T](Code.Obj.newConst[T](code))
      w.attr.put(Action.attrSource, codeObj)
      ()
    }
  }

  // ---- Widget ----

  def widgetGraphWithSource[T <: Txn[T]](c: blackbox.Context)(body: c.Expr[lucre.swing.graph.Widget])(tx: c.Expr[T])
                                        /* note: implicits _are_ used! */(implicit tt: c.WeakTypeTag[T]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("widget", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.WidgetMacroOps[T]]
      implicit val txc = tx.splice // don't annotate the type with `T`, it will break scalac
      val w                   = ext.`this`
      w.graph()               = Widget.GraphObj.newConst[T](Widget.Graph(body.splice))
      val code                = Widget.Code(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[T](Code.Obj.newConst[T](code))
      w.attr.put(Widget.attrSource, codeObj)
      ()
    }
  }
}