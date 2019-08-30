/*
 *  Macros.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.stm.Sys
import de.sciss.synth.SynthGraph

import scala.concurrent.stm.Ref
import scala.reflect.macros.blackbox
import scala.tools.nsc.Global
import scala.tools.nsc.interpreter.IMain

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

  def procGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[Unit])(tx: c.Expr[S#Tx])
                         /* note: implicits _are_ used! */ (implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("proc", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.ProcMacroOps[S]]
      implicit val txc = tx.splice // don't annotate the type with `S#Tx`, it will break scalac
      val p                   = ext.`this`
      p.graph()               = SynthGraphObj.newConst[S](SynthGraph(body.splice))
      val code                = Code.SynthGraph(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      p.attr.put(Proc.attrSource, codeObj)
    }
  }

  // ---- Control ----

  def controlGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[Unit])(tx: c.Expr[S#Tx])
                             /* note: implicits _are_ used! */(implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("control", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.ControlMacroOps[S]]
      implicit val txc = tx.splice // don't annotate the type with `S#Tx`, it will break scalac
      val w                   = ext.`this`
      w.graph()               = Control.GraphObj.newConst[S](Control.Graph(body.splice))
      val code                = Code.Control(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      w.attr.put(Control.attrSource, codeObj)
    }
  }

  // ---- Action ----

  def actionGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[lucre.expr.graph.Act])(tx: c.Expr[S#Tx])
                                        /* note: implicits _are_ used! */(implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("action", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.ActionMacroOps[S]]
      implicit val txc = tx.splice // don't annotate the type with `S#Tx`, it will break scalac
      val w                   = ext.`this`
      w.graph()               = Action.GraphObj.newConst[S](Action.Graph(body.splice))
      val code                = Code.Action(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      w.attr.put(Action.attrSource, codeObj)
    }
  }

  // ---- Widget ----

  def widgetGraphWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[lucre.swing.graph.Widget])(tx: c.Expr[S#Tx])
                                        /* note: implicits _are_ used! */(implicit tt: c.WeakTypeTag[S]): c.Expr[Unit] = {
    import c.universe._

    val source      = mkSource(c)("widget", body.tree)
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      val ext                 = c.prefix.splice.asInstanceOf[MacroImplicits.WidgetMacroOps[S]]
      implicit val txc = tx.splice // don't annotate the type with `S#Tx`, it will break scalac
      val w                   = ext.`this`
      w.graph()               = Widget.GraphObj.newConst[S](Widget.Graph(body.splice))
      val code                = Widget.Code(sourceExpr.splice)
      val codeObj             = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
      w.attr.put(Widget.attrSource, codeObj)
    }
  }

  // ---- ActionRaw ----

  private[this] val compileCount = Ref(0)

  private[this] var iMainImpl: IMain  = _
  private[this] var iMainPeer: Global = _

  def actionRawWithSource[S <: Sys[S]](c: blackbox.Context)(body: c.Expr[Action.Universe[S] => Unit])(tx: c.Expr[S#Tx])
                                      /* note: implicits _are_ used! */ (implicit tt: c.WeakTypeTag[S]): c.Expr[ActionRaw[S]] = {
    import c.universe._
    val source = body.tree match {
      case Function(ValDef(_, argName, _, _) :: Nil, _ /* funBody */) =>
        // println(s"ARG NAME '$argName'")
        val argNameS      = argName.toString
        val isUnderscore  = argNameS == "x$1"
        val needsAlias    = !isUnderscore && argNameS != "universe"
        val source0       = mkSource(c)("action", body.tree /* funBody */)
        val idx           = source0.indexOf("=>")
        val source1       = if (idx < 0) source0 else source0.substring(idx + 2)
        val source2       = indentSource(source1)
        if (!needsAlias) source2 else s"val $argNameS = universe\n$source2"
      case other =>
        c.error(body.tree.pos, s"Expecting a Function1 literal but got ${other.getClass.getName}")
        null
    }

    val nameIdx     = compileCount.single.transformAndGet(_ + 1)
    val nameTime    = System.currentTimeMillis()
    val name        = s"Action${nameTime}_$nameIdx"
    val code0       = Code.ActionRaw(source)

    val iMainOld    = iMainImpl
    val iMainPeerOld= iMainPeer
    val global      = c.universe
    val iMain       = if (iMainOld != null && iMainPeerOld == global) iMainOld else {
      require(global.isInstanceOf[Global], s"Universe not an instance of Global: $global")
      val newPeer = global.asInstanceOf[Global]
      val res     = MacroCompilerImpl(newPeer)
      iMainImpl   = res
      iMainPeer   = newPeer
      res
    }

    implicit val compiler: Code.Compiler = new CompilerImpl.Impl(iMain)

    val jar         = code0.execute(name)
    val jarS0       = new String(jar, "ISO-8859-1")
    val sourceExpr  = c.Expr[String](Literal(Constant(source)))
    val nameExpr    = c.Expr[String](Literal(Constant(name  )))
    val jarExpr     = c.Expr[String](Literal(Constant(jarS0 )))

    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
    reify {
      implicit val txc  = tx.splice // don't annotate the type with `S#Tx`, it will break scalac
      val codeObj       = Code.Obj.newVar[S](Code.Obj.newConst[S](Code.ActionRaw(sourceExpr.splice)))
      val a             = ActionRaw.newConst[S](nameExpr.splice, jarExpr.splice.getBytes("ISO-8859-1"))
      a.attr.put(ActionRaw.attrSource, codeObj)
      a
    }
  }
}