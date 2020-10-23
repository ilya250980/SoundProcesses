package de.sciss.synth.proc
package gui

import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.InMemory
import de.sciss.span.Span
import de.sciss.submin.Submin

import scala.swing.{MainFrame, Swing}

object TransportViewTest {
  type S = InMemory
  type T = InMemory.Txn

  def main(args: Array[String]): Unit = {
    Submin.install(true)

    implicit val system: S = InMemory()
    implicit val universe: Universe[T] = system.step { implicit tx => Universe.dummy }

    val sr    = TimeRef.SampleRate
    val span  = Span(0L, (sr * 60 * 10).toLong)
    val model = TimelineModel(span, span, span, sr)
    model.selection = Span(0L, span.length >> 1)

    val view = system.step { implicit tx =>
      val transport = Transport[T](universe)
      TransportView[T](transport, model)
    }

    Swing.onEDT {
      new MainFrame {
        title = "TransportView Test"
        contents = view.component
        pack().centerOnScreen()
        open()
      }
    }
  }
}