package de.sciss
package synth
package proc

import de.sciss.lucre.expr.{BooleanObj, SpanLikeObj}
import de.sciss.lucre.stm.{Disposable, Identifier}
import de.sciss.lucre.{event => evt}
import de.sciss.span.Span

/*
  To run only this suite:

  test-only de.sciss.synth.proc.Issue15

  */
class Issue15 extends ConfluentEventSpec {
  final val DEBUG = false

  ignore /* "AttrMap" */ should "dispatch events after repeated listener (un)registration" in { system =>
    val obs = new Observation

    if (DEBUG) de.sciss.lucre.event.showLog = true

    // ---- we create the "workspace" ----
    val (fH, pObjH, tlH, timedIDH, spanH) = system.step { implicit tx =>
      val p         = Proc[S]
      val pObj      = p // Obj(Proc.Elem(p))
      // pObj.attr // initialize for debugger
      val tl        = Timeline[S]
      val span      = SpanLikeObj.newConst[S](Span(0L, 10000L)): SpanLikeObj[S] // Expr[S, SpanLike]
      val timed     = tl.add(span, pObj)
      val _pObjH    = tx.newHandle(pObj)
      val _tlH      = tx.newHandle(tl)
      // import de.sciss.lucre.synth.expr.IdentifierSerializer
      val _timedIDH = tx.newHandle(timed.id)(Identifier.serializer[S])
      // import SpanLikeObj.serializer
      val _spanH    = tx.newHandle(span)
      val f         = Folder[S]
      val tlObj     = tl // Obj(Timeline.Elem(tl))
      f.addLast(tlObj)
      implicit val fSer = Folder.serializer[S]
      val _fH       = tx.newHandle(f)
      (_fH, _pObjH, _tlH, _timedIDH, _spanH)
    }

    def assertChildren(header: String, size: Int)(implicit tx: S#Tx): Unit = {
      val tl = tlH()
      val ch = de.sciss.lucre.event.Peek.targets(tl.asInstanceOf[evt.Node[S]])
      assert(ch.size === size)
      if (DEBUG) {
        println(s"\n---- $header ----")
        ch.foreach(println)
        println()
      }
    }

    // ---- we "open the folder" view; this is crucial for the bug to appear ----
    system.step { implicit tx =>
      val f = fH()
      f.changed.react { implicit tx => upd =>
        // nada
      }
    }

    def timelineObservation(): Disposable[S#Tx] = system.step { implicit tx =>
      val tl   = tlH()
      val _obs = tl.changed.react(obs.register)
      obs.assertEmpty()
      assertChildren(s"AFTER TL OBSERVATION (${_obs})", 4)
      _obs
    }

    // ---- we "open the timeline" view ----
    val obs1 = timelineObservation()

    def muteObservation(): Unit = {
      /* val muteH = */ system.step { implicit tx =>
        assertChildren("BEFORE FIRST MUTATION", 4)

        val pObj    = pObjH()
//        val tl      = tlH()
        val muteObj = BooleanObj.newConst[S](true) : BooleanObj[S]
        // val timed   = BiGroup.Entry(timedIDH(), spanH(), pObj)
        pObj.attr.put(ObjKeys.attrMute, muteObj)
        obs.assertEquals()
//        BiGroup.Update(tl, Vec(
//          BiGroup.ElementMutated(timed, Obj.UpdateT(pObj, Vec(
//            Obj.AttrAdded(ObjKeys.attrMute, muteObj)
//          )))
//        ))

        assertChildren("AFTER FIRST MUTATION", 4)

        // import BooleanObj.serializer
        tx.newHandle(muteObj)
      }

      system.step { implicit tx =>
        assertChildren("BEFORE SECOND MUTATION", 4)

        val pObj    = pObjH()
//        val tl      = tlH()
//        val muteObj = muteH()
        // val timed   = BiGroup.Entry(timedIDH(), spanH(), pObj)
        pObj.attr.remove(ObjKeys.attrMute)
        obs.assertEquals()
//        BiGroup.Update(tl, Vec(
//          BiGroup.ElementMutated(timed, Obj.UpdateT(pObj, Vec(
//            Obj.AttrRemoved(ObjKeys.attrMute, muteObj)
//          )))
//        ))

        assertChildren("AFTER SECOND MUTATION", 4)
      }
    }

    // ---- we "mute and un-mute" ----
    muteObservation()

    if (DEBUG) {
      de.sciss.lucre.confluent.showLog = true
      de.sciss.lucre.stm      .showLog = true
    }

    // ---- we "close" the timeline view; this produces the illegal state somehow ----
    system.step { implicit tx =>
      assertChildren("BEFORE DISPOSAL", 4)
      obs1.dispose()
      assertChildren("AFTER DISPOSAL", 3)
    }

    // ---- we "open" the view again ----
    /* val obs2 = */ timelineObservation()

    // ---- we "mute and un-mute"; this is were the bug occurs ----
    muteObservation()
  }
}