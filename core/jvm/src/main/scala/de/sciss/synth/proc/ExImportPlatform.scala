package de.sciss.synth.proc

import de.sciss.lucre.expr.graph.{AudioFileSpec, Ex, Obj, AudioCue => _AudioCue}
import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.expr.graph.Obj
import de.sciss.synth.proc

trait ExImportPlatform {
  implicit def audioCueType: Obj.Bridge[AudioCue] with Obj.CanMake[AudioCue] with HasDefault[AudioCue] =
    _AudioCue.Type

  type AudioCue       = proc              .AudioCue
}
