package de.sciss.synth.proc

import de.sciss.lucre.stm.InMemory
import de.sciss.lucre.{expr, swing}

import scala.swing.Component

object ResampleDlgTest {
  protected def mkView(): Component = {
    import expr.ExOps._
    import swing.graph._
    val g = swing.Graph {
      //      val sepWave         = TitledSeparator("Waveform I/O")
      val sepWave         = Label("———— Waveform I/O ————") // Titled-Border
      val lbIn            = Label("Input file:")
      val ggIn            = PathField()
      ggIn.mode           = PathField.Open
      ??? // ggIn.value() ---> "in".attr[File]
      val ggInfo          = TextField(20)
      ggInfo.editable     = false
      ggInfo.focusable    = false
      //      ggIn.info           = true
      val lbOut           = Label("Output file:")
      val ggOut           = PathField()
      ggOut.mode          = PathField.Save
      val ggFileType      = ComboBox(List("AIFF", "Snd", "IRCAM", "WAVE", "Wav64"))
      val ggSmpFmt        = ComboBox(List("16-bit int", "24-bit int", "32-bit float", "32-bit int"))
      ggSmpFmt.index()    = 1
      //      ggSmpFmt.valueOption = Some("24-bit int")
      val lbGain          = Label("Gain:")
      val ggGain          = DoubleField()
      ggGain.value()      = -0.20
      ggGain.max          = 1000.0
      //      val ggGain          = IntField()
      //      ggGain.value()      = -20
      //      ggGain.min          = -1000
      //      ggGain.max          = 1000
      ggGain.unit         = "dB"
      //      ggGain.spec         = ParamSpec(...)
      val ggGainType      = ComboBox(List("normalized", "immediate"))
      val sepSRC          = Label("———— Sample Rate Conversion ————") // Titled-Border
      val lbNewRate       = Label("New rate:")
      //      val ggNewRate       = NumberField()
      val ggModRate       = CheckBox()
      ggModRate.tooltip   = "Modulate resampling rate"
      val lbDistinctRight = Label("Distinct right channel mod.:")
      val ggDistinctRight = CheckBox()
      ggDistinctRight.tooltip = "Use a different envelope for the rightmost channel"
      ggDistinctRight.enabled = false
      val ggChangePch     = CheckBox("Change Pitch/Speed")
      ggChangePch.tooltip = "If checked, changes the output file's nominal sample rate"
      ggChangePch.selected() = true
      val lbFltLen        = Label("FIR length:")
      val ggFltLen        = ComboBox(List("Short", "Medium", "Long"))
      ggFltLen.index()    = 1
      val ggInterp        = CheckBox("Interpolate")
      ggInterp.tooltip    = "Increase resolution by interpolating the FIR table"
      val ggProg          = ProgressBar()
      //      ggProg.max          = 300
      val ggCancel        = Button(" X ")
      ggCancel.enabled    = false
      val ggRender        = Button(" Render ")
      //      ggRender.action     = "render".attr

      val lineIn    = FlowPanel(lbIn, ggIn)
      val lineOut   = FlowPanel(lbOut, ggOut)
      val lineFmt   = FlowPanel(ggFileType, ggSmpFmt)
      val lineGain  = FlowPanel(lbGain, ggGain, ggGainType)
      val lineRate  = FlowPanel(lbNewRate, ggModRate)
      val lineRate2 = FlowPanel(lbDistinctRight, ggDistinctRight)
      val lineMode  = FlowPanel(ggChangePch, lbFltLen, ggFltLen, ggInterp)
      val lineProg  = FlowPanel(ggProg, ggCancel, ggRender)
      val p = GridPanel(
        sepWave, lineIn, ggInfo, lineOut, lineFmt, lineGain, lineRate, lineRate2, sepSRC, lineMode, lineProg
      )
      p.columns = 1
      p
    }

    type              S = InMemory
    implicit val sys: S = InMemory()

    val view = sys.step { implicit tx =>
      g.expand[S]()
    }
    view.component
  }
}