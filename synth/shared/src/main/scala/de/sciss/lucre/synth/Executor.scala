package de.sciss.lucre.synth

object Executor extends ExecutorPlatform {
  trait Cancelable {
    def cancel(): Unit
  }
}
