package de.sciss.synth.proc

import de.sciss.synth.proc.impl.AuralProcFactoryImpl

trait SoundProcessesPlatform {
  protected def initPlatform(): Unit = _initPlatform

  private lazy val _initPlatform: Unit = {
    AuralProcFactoryImpl.init()
  }
}
