package de.sciss.synth.proc.tests

import de.sciss.lucre.synth.Txn
import de.sciss.lucre.{Cursor, DoubleObj, IntObj, LongObj}
import de.sciss.span.Span
import de.sciss.synth
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AuralContext, EnvSegment, Grapheme, Proc, Timeline, Transport, graph}

object AuralTests2 extends AuralTestLike.Factory {
  def main(args: Array[String]): Unit = init(args)

  def run[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]): Unit = {
    new AuralTests2[T](name)
    ()
  }
}
class AuralTests2[T <: Txn[T]](name: String)(implicit cursor: Cursor[T]) extends AuralTestLike[T] {
  def run()(implicit context: AuralContext[T]): Unit =
    name match {
      case "--test1" => test1()
//      case "--test2" => test2()
      case "--test3" => test3()
      case "--test4" => test4()
      case "--test5" => test5()
      case _         =>
        println("WARNING: No valid option given, using --test1")
        test1()
    }

  import synth._
  import ugen._


  ////////////////////////////////////////////////////////////////////////////////////// 5

  def test5()(implicit context: AuralContext[T]): Unit = {
    println("----test3----")
    println(
      """
        |Expected behaviour:
        |A steady sine of 400 Hz is heard for one second, then raises
        |for six seconds; the original target at 10 seconds is replaced
        |by an interpolated level at 7 seconds. At 7 seconds tone is
        |steady 800 Hz.
        |
        |""".stripMargin)

    cursor.step { implicit tx =>
      val _view = procV {
        import graph.Ops._
        val freq  = "freq".ar
        val osc   = SinOsc.ar(freq) * 0.2
        Out.ar(0, Pan2.ar(osc))
      }

      val gr = Grapheme[T]()
      val f1 =  400
      val f2 = 1000
      val t0 =  1.0
      val t1 =  7.0
      val t2 = 10.0
      gr.add(frame(0.0), IntObj.newConst(400))
      gr.add(frame(t0) , EnvSegment.Obj.newConst(EnvSegment.Single( 400, Curve.lin)))
      val t2Obj = frame(t2): LongObj[T]
      val t2H   = tx.newHandle(t2Obj)
      val ev    = EnvSegment.Single(1000, Curve.lin): EnvSegment.Obj[T]
      val evH   = tx.newHandle(ev)
      gr.add(t2Obj, ev)

      val attr = _view.obj.attr
      attr.put("freq", gr)
      _view.play()

      val grH = tx.newHandle(gr)

      after(4.0) { implicit tx =>
        val gr  = grH()
        val lvl = t1.linLin(t0, t2, f1, f2)  // = 800
        println(f"Insert static $lvl%g")
        gr.add(frame(t1), DoubleObj.newConst(lvl))
//        gr.remove()
        after(3.0) { implicit tx =>
          println("Now static")
          after(1.0) { implicit tx =>
            println("Removing last point")
            val gr = grH()
            gr.remove(t2H(), evH())
            ()
          }
        }
      }

      after(11.0) { implicit tx =>
        context.server.peer.dumpTree()
      }

      stopAndQuit(12.0)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////// 4

  def test4()(implicit context: AuralContext[T]): Unit = {
    println("----test3----")
    println(
      """
        |Expected behaviour:
        |We construct a grapheme with envelope segments, audible
        |as an oscillator frequency going exponentially up for 3 seconds,
        |then, as the ceil node changes, jumping down and descending to 200 Hz, for 3 seconds,
        |then going up to 400 Hz for 3 seconds.
        |Final node dump should not contain any 'env' named synth.
        |
        |""".stripMargin)

    cursor.step { implicit tx =>
      val _view = procV {
        import graph.Ops._
        val freq  = "freq".ar
        val osc   = SinOsc.ar(freq) * 0.2
        Out.ar(0, Pan2.ar(osc))
      }

      val gr = Grapheme[T]()
      gr.add(frame(0.0), EnvSegment.Obj.newConst(EnvSegment.Single( 600, Curve.exp)))
      val ev = EnvSegment.Obj.newVar(EnvSegment.Single(1000, Curve.lin))
      val evH = tx.newHandle(ev)
      gr.add(frame(6.0), ev)
      gr.add(frame(9.0), IntObj.newConst(400))

      val attr = _view.obj.attr
      attr.put("freq", gr)
      _view.play()

      after(3.0) { implicit tx =>
        println("Start from low")
        val ev = evH()
        ev() = EnvSegment.Single(200, Curve.lin)
      }

      after(11.0) { implicit tx =>
        context.server.peer.dumpTree()
      }

      stopAndQuit(12.0)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////// 3

  def test3()(implicit context: AuralContext[T]): Unit = {
    println("----test4----")
    println(
      """
        |Expected behaviour:
        |We construct a grapheme with envelope segments, audible
        |as an oscillator frequency going exponentially up for 3 seconds, then down linearly for 3 seconds,
        |then staying there.
        |
        |""".stripMargin)

    cursor.step { implicit tx =>
      val _view = procV {
        import graph.Ops._
        val freq  = "freq".ar
        val osc   = SinOsc.ar(freq) * 0.2
        Out.ar(0, Pan2.ar(osc))
      }

      val gr = Grapheme[T]()
      gr.add(frame(0.0), EnvSegment.Obj.newConst(EnvSegment.Single( 400, Curve.exp)))
      gr.add(frame(3.0), EnvSegment.Obj.newConst(EnvSegment.Single(1000, Curve.lin)))
      gr.add(frame(6.0), IntObj.newConst(400))

      val attr = _view.obj.attr
      attr.put("freq", gr)
      _view.play()
      stopAndQuit(9.0)
    }
  }

  ////////////////////////////////////////////////////////////////////////////////////// 1

  def test1()(implicit context: AuralContext[T]): Unit = {
    println("----test1----")
    println(
      """
        |Expected behaviour:
        |We set up a source and sink and
        |timeline connections between them.
        |Two seconds after start, we hear pink noise.
        |
        |""".stripMargin)

    cursor.step { implicit tx =>
      val tl = Timeline[T]()

      val pGen = proc {
        val sig = PinkNoise.ar(Seq(1, 1))
        graph.ScanOut(sig)
      }
      pGen.name = "gen"
      val genOut = pGen.outputs.add(Proc.mainOut)

      val pDif = proc {
        val sig = graph.ScanInFix(2)
        Out.ar(0, sig * 0.2)
      }
      pDif.name = "dif"
      val difIn = Timeline[T]()
      pDif.attr.put(Proc.mainIn, difIn)

      difIn.add(Span.from(frame(0.0)), genOut)
      tl.add(Span(frame(2.0), frame(9.0)), pGen)
      tl.add(Span(frame(2.0), frame(8.0)), pDif)
      //      tl.add(Span.from(frame(2.0)), pGen)
      //      tl.add(Span.from(frame(2.0)), pDif)

      val t = Transport[T](context)
      t.addObject(tl)
      t.play()

      stopAndQuit(4.0)
    }
  }
}