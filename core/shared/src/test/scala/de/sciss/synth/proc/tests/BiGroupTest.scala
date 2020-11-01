package de.sciss.synth.proc.tests

import de.sciss.lucre.{BiGroup, Cursor, LongObj, SpanLikeObj, Txn}
import de.sciss.lucre.synth.InMemory
import de.sciss.span.{Span, SpanLike}

object BiGroupTest {
  type I = InMemory.Txn

  def apply(): BiGroupTest[I] = new BiGroupTest[I](InMemory())
}
class BiGroupTest[T <: Txn[T]](cursor: Cursor[T]) /* extends ExprImplicits[S] */ {

  def t[A](fun: T => A): A = cursor.step(fun)

  val bi = t { implicit tx =>
    val res = BiGroup.Modifiable[T, LongObj]
    res.changed.react { _ => upd =>
      println(s"Observed: $upd")
    }
    res
  }

  def add(span: SpanLike = Span(33, 44), elem: Long = 55): Unit =
    t { implicit tx =>
      bi.add(span, elem)
      ()
    }

  def addValVar(span: SpanLike = Span(33, 44), init: Long = 66): LongObj.Var[T] =
    t { implicit tx =>
      val elem = LongObj.newVar[T](init)
      bi.add(span, elem)
      elem
    }

  def addKeyVar(init: SpanLike = Span(33, 44), elem: Long = 77): SpanLikeObj.Var[T] =
    t { implicit tx =>
      val span = SpanLikeObj.newVar[T](init)
      bi.add(span, elem)
      span
    }

  def at(time: Long) = t { implicit tx =>
    bi.intersect(time).toIndexedSeq
  }

  def within(span: SpanLike) = t { implicit tx =>
    bi.intersect(span).toIndexedSeq
  }

  def after(time: Long) = t { implicit tx =>
    bi.eventAfter(time)
  }

  def before(time: Long) = t { implicit tx =>
    bi.eventBefore(time)
  }

  def eventsAt(time: Long) = t { implicit tx =>
    val (a, b) = bi.eventsAt(time)
    (a.toIndexedSeq, b.toIndexedSeq)
  }

  def list(): Unit = {
    val li = t { implicit tx =>
      bi.debugList
    }
    println(li)
  }
}
