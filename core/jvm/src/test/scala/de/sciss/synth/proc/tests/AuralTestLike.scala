package de.sciss.synth.proc.tests

import de.sciss.log.Level
import de.sciss.lucre.store.BerkeleyDB
import de.sciss.lucre.synth.{Server, Txn => STxn}
import de.sciss.lucre.{Cursor, DoubleObj, Folder, Obj, SpanLikeObj, synth}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.SynthGraph
import de.sciss.synth.proc.{AuralContext, AuralObj, AuralSystem, Confluent, Durable, Proc, SoundProcesses, TimeRef, Timeline, Universe}
import de.sciss.synth.proc.SoundProcesses.{logAural, logTransport}

import scala.concurrent.stm.{Txn => STMTxn}
import scala.language.implicitConversions

object AuralTestLike {
  trait Factory {
    val confluent = false /* true */  // currently test4 has a problem with event-variables in confluent

    protected def run[T <: STxn[T]](name: String)(implicit cursor: Cursor[T]): Unit

    def init(args: Array[String]): Unit = {
      SoundProcesses.init()

      val name = args.headOption.getOrElse("?")

      if (confluent) {
        type T  = Confluent.Txn
        val sys = Confluent(BerkeleyDB.tmp())
        val (_, cursor) = sys.cursorRoot(_ => ())(implicit tx => _ => sys.newCursor())
        run[T](name)(cursor)

      } else {
        type T  = Durable.Txn
        val sys = Durable(BerkeleyDB.tmp())
        val cursor: Cursor[T] = sys
        run[T](name)(cursor)
      }
    }
  }
}
abstract class AuralTestLike[T <: synth.Txn[T]](implicit cursor: Cursor[T]) {
  // ---- abstract ----

  protected def run()(implicit context: AuralContext[T]): Unit

  // ---- impl ----

  logAural    .level  = Level.Debug
  logTransport.level  = Level.Debug
  // de.sciss.lucre.synth.showLog = true

  implicit val universe: Universe[T] = cursor.step { implicit tx =>
    val res = Universe.dummy[T]
    val as =  res.auralSystem
    as.whenStarted(initView)
    as.start()
    res
  }

  final val as: AuralSystem = universe.auralSystem

  final def initView(s: Server): Unit = {
    if (STMTxn.findCurrent.isDefined) {
      Console.err.println("Damn! I could swear there is no transaction.")
      throw new IllegalStateException()
    }

    s.peer.dumpOSC()
    implicit val context: AuralContext[T] = cursor.step { implicit tx =>
      AuralContext[T](s)
    }

    run()
  }

  final def after(secs: Double)(code: T => Unit): Unit = {
    val t = new Thread {
      override def run(): Unit = {
        Thread.sleep((secs * 1000).toLong)
        cursor.step { implicit tx =>
          code(tx)
        }
      }
    }
    STMTxn.findCurrent.fold(t.start()) { implicit tx =>
      STMTxn.afterCommit(_ => t.start())
    }
  }

  final def quit()(implicit tx: T): Unit =
    tx.afterCommit {
      Thread.sleep(1000)  // have to wait a bit for scsynth to quit
      scala.sys.exit()
    }

  final def procV(graph: => Any)(implicit tx: T, context: AuralContext[T]): AuralObj.Proc[T] = {
    val pObj  = proc(graph)
    val _view = AuralObj(pObj)
    _view.asInstanceOf[AuralObj.Proc[T]]
  }

  final def proc(graph: => Any)(implicit tx: T): Proc[T] = {
    val p = Proc[T]()
    val g = SynthGraph {
      graph
      ()
    }
    p.graph() = Proc.GraphObj.newConst[T](g)
    p // Obj(Proc.Elem(p))
  }

  final def timelineV()(implicit tx: T, context: AuralContext[T]): AuralObj.Timeline[T] = {
    val tlObj = timeline()
    val _view = AuralObj.Timeline(tlObj)
    _view
  }

  final def timeline()(implicit tx: T): Timeline[T] = {
    val tl    = Timeline[T]()
    tl // Obj(Timeline.Elem(tl))
  }

  final def frame  (secs  : Double): Long   = (secs  * TimeRef.SampleRate).toLong
  final def seconds(frames: Long  ): Double = frames / TimeRef.SampleRate

  final def putDouble(proc: Proc[T], key: String, value: Double)(implicit tx: T): Unit = {
    // val imp = ExprImplicits[T]
    // import imp._
    proc.attr.put(key, value: DoubleObj[T])
    ()
  }

  final def stopAndQuit(delay: Double = 4.0): Unit =
    after(delay) { implicit tx =>
      as.stop()
      quit()
    }

  final def addScanIn(proc: Proc[T], key: String = "in"): (Proc[T], String) /* Scan[T] */ = {
    (proc, key) // proc.inputs.add(key)
  }

  final def addOutput(proc: Proc[T], key: String = "out")(implicit tx: T): Proc.Output[T] = {
    proc.outputs.add(key)
  }

  implicit final class OutputOps(val `this`: Proc.Output[T]) /* extends AnyVal */ {
    def ~> (that: (Proc[T], String))(implicit tx: T): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).fold[Unit] {
        attr.put(key, `this`)
        ()
      } {
        case f: Folder[T] => f.addLast(`this`)
        case prev =>
          val f = Folder[T]()
          f.addLast(prev)
          f.addLast(`this`)
          attr.put(key, f)
          ()
      }
    }

    def ~/> (that: (Proc[T], String))(implicit tx: T): Unit = {
      val (sink, key) = that
      val attr = sink.attr
      attr.get(key).getOrElse(sys.error(s"Attribute $key was not assigned")) match {
        case `sink` =>
          attr.remove(key)
          ()
        case f: Folder[T] =>
          val idx = f.indexOf(`this`)
          if (idx < 0) sys.error(s"Attribute $key has a folder but does not contain ${`this`}")
          f.removeAt(idx)
          ()

        case other => sys.error(s"Cannot remove output from $other")
      }
    }
  }

  implicit final def timeRange(in: (Double, Double)): Span = {
    val start = (in._1 * TimeRef.SampleRate).toLong
    val stop  = (in._2 * TimeRef.SampleRate).toLong
    Span(start, stop)
  }

  implicit final class TimelineOps(tl: Timeline[T]) /* extends AnyVal */ {
    def add(span: SpanLike, obj: Obj[T])(implicit tx: T): Unit = {
      val tlm = tl.modifiableOption.get  // yo
      tlm.add(SpanLikeObj.newConst(span), obj)
      ()
    }

    def remove(span: SpanLike, obj: Obj[T])(implicit tx: T): Unit = {
      val tlm = tl.modifiableOption.get  // yo
      val res = tlm.remove(SpanLikeObj.newConst(span), obj)
      if (!res) Console.err.println(s"Warning: object $obj at $span not found in timeline")
    }
  }
}