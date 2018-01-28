package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.ugen.ControlValues

trait ControlValuesView[S <: Sys[S]]
  extends Observable[S#Tx, Option[ControlValues]] with stm.Source[S#Tx, Option[ControlValues]]
