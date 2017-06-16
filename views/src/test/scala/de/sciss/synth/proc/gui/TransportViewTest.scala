package de.sciss.synth.proc.gui

import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.InMemory
import de.sciss.span.Span
import de.sciss.submin.Submin
import de.sciss.synth.proc.{AuralSystem, TimeRef, Transport, WorkspaceHandle}

import scala.swing.{MainFrame, Swing}

object TransportViewTest {
  type S = InMemory

  def main(args: Array[String]): Unit = {
    Submin.install(true)

    implicit val system: S = InMemory()
    implicit val ws: WorkspaceHandle[S] = WorkspaceHandle.Implicits.dummy

    val aural = AuralSystem()
    val sr    = TimeRef.SampleRate
    val span  = Span(0L, (sr * 60 * 10).toLong)
    val model = TimelineModel(span, sr)
    model.selection = Span(0L, span.length >> 1)

    val view = system.step { implicit tx =>
      val transport = Transport[S](aural)
      TransportView[S](transport, model)
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