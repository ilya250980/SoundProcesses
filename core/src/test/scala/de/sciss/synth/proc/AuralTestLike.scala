package de.sciss.synth.proc

import de.sciss.lucre.expr.{DoubleObj, SpanLikeObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Sys}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.SynthGraph

import scala.concurrent.stm.Txn
import scala.language.implicitConversions

object AuralTestLike {
  trait Factory {
    val confluent = false /* true */  // currently test4 has a problem with event-variables in confluent

    protected def run[S <: Sys[S]](name: String)(implicit cursor: stm.Cursor[S]): Unit

    def init(args: Array[String]): Unit = {
      SoundProcesses.init()

      val name = args.headOption.getOrElse("?")

      if (confluent) {
        type S  = Confluent
        val sys = Confluent(BerkeleyDB.tmp())
        val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
        run[S](name)(cursor)

      } else {
        type S  = Durable
        val sys = Durable(BerkeleyDB.tmp())
        val cursor: stm.Cursor[S] = sys
        run[S](name)(cursor)
      }
    }
  }
}
abstract class AuralTestLike[S <: Sys[S]](implicit cursor: stm.Cursor[S]) {
  // ---- abstract ----

  protected def run()(implicit context: AuralContext[S]): Unit

  // ---- impl ----

  showAuralLog      = true
  showTransportLog  = true
  // de.sciss.lucre.synth.showLog = true

  final val as: AuralSystem = AuralSystem()
  cursor.step { implicit tx =>
    as.whenStarted(initView)
    as.start()
  }

  final def initView(s: Server): Unit = {
    if (Txn.findCurrent.isDefined) {
      Console.err.println("Damn! I could swear there is no transaction.")
      throw new IllegalStateException()
    }

    s.peer.dumpOSC()
    implicit val context: AuralContext[S] = cursor.step { implicit tx =>
      import WorkspaceHandle.Implicits._
      AuralContext[S](s)
    }

    run()
  }

  final def after(secs: Double)(code: S#Tx => Unit): Unit = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep((secs * 1000).toLong)
        cursor.step { implicit tx =>
          code(tx)
        }
      }
    }
    Txn.findCurrent.fold(t.start()) { implicit tx =>
      Txn.afterCommit(_ => t.start())
    }
  }

  final def quit()(implicit tx: S#Tx): Unit =
    tx.afterCommit {
      Thread.sleep(1000)  // have to wait a bit for scsynth to quit
      scala.sys.exit()
    }

  final def procV(graph: => Unit)(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Proc[S] = {
    val pObj  = proc(graph)
    val _view = AuralObj.Proc(pObj)
    _view
  }

  final def proc(graph: => Unit)(implicit tx: S#Tx): Proc[S] = {
    val p = Proc[S]
    val g = SynthGraph {
      graph
    }
    p.graph() = SynthGraphObj.newConst[S](g)
    p // Obj(Proc.Elem(p))
  }

  final def timelineV()(implicit tx: S#Tx, context: AuralContext[S]): AuralObj.Timeline[S] = {
    val tlObj = timeline()
    val _view = AuralObj.Timeline(tlObj)
    _view
  }

  final def timeline()(implicit tx: S#Tx): Timeline[S] = {
    val tl    = Timeline[S]
    tl // Obj(Timeline.Elem(tl))
  }

  final def frame  (secs  : Double): Long   = (secs  * TimeRef.SampleRate).toLong
  final def seconds(frames: Long  ): Double = frames / TimeRef.SampleRate

  final def putDouble(proc: Proc[S], key: String, value: Double)(implicit tx: S#Tx): Unit = {
    // val imp = ExprImplicits[S]
    // import imp._
    proc.attr.put(key, value: DoubleObj[S])
  }

  final def stopAndQuit(delay: Double = 4.0): Unit =
    after(delay) { implicit tx =>
      as.stop()
      quit()
    }

  final def addScanIn(proc: Proc[S], key: String = "in"): (Proc[S], String) /* Scan[S] */ = {
    (proc, key) // proc.inputs.add(key)
  }

  final def addOutput(proc: Proc[S], key: String = "out")(implicit tx: S#Tx): Output[S] = {
    proc.outputs.add(key)
  }

  implicit final class OutputOps(val `this`: Output[S]) /* extends AnyVal */ {
    def ~> (that: (Proc[S], String))(implicit tx: S#Tx): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).fold[Unit] {
        attr.put(key, `this`)
      } {
        case f: Folder[S] => f.addLast(`this`)
        case prev =>
          val f = Folder[S]
          f.addLast(prev)
          f.addLast(`this`)
          attr.put(key, f)
      }
    }

    def ~/> (that: (Proc[S], String))(implicit tx: S#Tx): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).getOrElse(sys.error(s"Attribute $key was not assigned")) match {
        case `sink` => attr.remove(key)
        case f: Folder[S] =>
          val idx = f.indexOf(`this`)
          if (idx < 0) sys.error(s"Attribute $key has a folder but does not contain ${`this`}")
          f.removeAt(idx)

        case other => sys.error(s"Cannot remove output from $other")
      }
    }
  }

  implicit final def timeRange(in: (Double, Double)): Span = {
    val start = (in._1 * TimeRef.SampleRate).toLong
    val stop  = (in._2 * TimeRef.SampleRate).toLong
    Span(start, stop)
  }

  implicit final class TimelineOps(tl: Timeline[S]) /* extends AnyVal */ {
    def += (span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val tlm = tl.modifiableOption.get  // yo
      tlm.add(SpanLikeObj.newConst(span), obj)
    }

    def -= (span: SpanLike, obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val tlm = tl.modifiableOption.get  // yo
      val res = tlm.remove(SpanLikeObj.newConst(span), obj)
      if (!res) Console.err.println(s"Warning: object $obj at $span not found in timeline")
    }
  }
}