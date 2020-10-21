package de.sciss.synth.proc

import de.sciss.lucre
import de.sciss.lucre.{Artifact, ArtifactLocation}
import de.sciss.synth.proc.impl.AuralProcFactoryImpl

trait SoundProcessesPlatform {
  protected def initPlatform(): Unit = _initPlatform

  private lazy val _initPlatform: Unit = {
    Artifact        .init()
    ArtifactLocation.init()
    AudioCue        .init()

    lucre.expr.graph.Artifact         .init()
    lucre.expr.graph.ArtifactLocation .init()
    lucre.expr.graph.AudioCue         .init()
    lucre.expr.graph.Timeline         .init()

    AuralProcFactoryImpl.init()
  }
}
