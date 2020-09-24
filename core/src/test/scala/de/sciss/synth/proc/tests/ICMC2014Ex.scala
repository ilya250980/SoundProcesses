package de.sciss.synth.proc.tests

import de.sciss.lucre.expr.SpanExtensions
import de.sciss.lucre.{LongObj, SpanObj, Txn}
import de.sciss.span.Span

import scala.language.implicitConversions

trait ICMC2014Ex[T <: Txn[T]] {
  def any2stringadd: Any = ()

//  val imp = ExprImplicits[S]
//  import imp._
  implicit def spanOps2(span: Span.type): SpanExtensions.Ops2 =
    new SpanExtensions.Ops2(span)

  def placeAfter(pred: SpanObj.Var[T],
                 succ: SpanObj.Var[T],
                 gap : LongObj    [T])
                (implicit tx: T): Unit = {
    val newStart = pred.stop + gap
    val newStop  = newStart + succ().length
    succ()       = Span.apply(newStart, newStop)
  }
}
