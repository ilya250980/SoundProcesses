//package de.sciss.synth.proc
//package impl
//
//import de.sciss.synth.SynthGraph
//
//import scala.reflect.macros.blackbox
//
//object Macros {
//  private def mkSource(c: blackbox.Context)(body: c.Expr[Any]): c.Expr[String] = {
//    val pos         = body.tree.pos
//    val fullSource  = pos.source.content
//    val source0     = if (pos.isRange) new String(fullSource.slice(pos.start, pos.start + pos.end - pos.start)) else {
//      // c.error(pos, s"Could not extract source")
//      "// could not extract source"
//    }
//    val source1 = source0.trim
//    val source2 = if (source1.startsWith("{") && source1.endsWith("}")) source1.substring(1, source1.length - 1) else source1
//    val res = {
//      val arr0 = source2.split("\n")
//      val arr1 = arr0.dropWhile(_.trim.isEmpty)
//      val arr  = arr1.reverse.dropWhile(_.trim.isEmpty).reverse
//      if (arr.isEmpty) source2 else {
//        val drop = arr.map(_.prefixLength(_ == ' ')).min
//        arr.map(_.substring(drop)).mkString("\n")
//      }
//    }
//
//    import c.universe._
//    c.Expr(Literal(Constant(res)))
//  }
//
//  def procGraphWithSource(c: blackbox.Context)(body: c.Expr[Unit])(tx: c.Expr[Any]): c.Expr[Unit] = {
//    import c.universe._
//
//    val source = mkSource(c)(body)
//    // N.B. the cast to `InMemory` doesn't seem to cause any problems, it's just to satisfy
//    // scalac at this point, although `ProcCompilerOps` has already put all type checks into place.
//    // There is simply no way (?) to get hold of the `S` parameter in the macro implementation.
//    import de.sciss.lucre.stm.{InMemory => S}
//    reify {
//      val ext                 = c.prefix.splice.asInstanceOf[Proc[S]]
//      implicit val txc: S#Tx  = tx.splice.asInstanceOf[S#Tx]
//      val p                   = ext.`this`
//      p.graph()               = SynthGraphObj.newConst[S](SynthGraph(body.splice))
//      val code                = Code.SynthGraph(source.splice)
//      val codeObj             = Code.Obj.newVar[S](Code.Obj.newConst[S](code))
//      p.attr.put(Proc.attrSource, codeObj)
//    }
//  }
//}