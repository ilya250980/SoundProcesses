package de.sciss.synth.proc.tests

import de.sciss.lucre.expr
import de.sciss.lucre.expr.{LongObj, SpanObj}
import de.sciss.lucre.stm.Sys
import de.sciss.span.Span

import scala.language.implicitConversions

trait ICMC2014Ex[S <: Sys[S]] {
//  val imp = ExprImplicits[S]
//  import imp._
  import expr.Ops._
  implicit def spanOps2(span: Span.type): expr.SpanExtensions.Ops2 =
    new expr.SpanExtensions.Ops2(span)

  def placeAfter(pred: SpanObj.Var[S],
                 succ: SpanObj.Var[S],
                 gap : LongObj    [S])
                (implicit tx: S#Tx): Unit = {
    val newStart = pred.stop + gap
    val newStop  = newStart + succ().length
    succ()       = Span.apply(newStart, newStop)
  }
}
