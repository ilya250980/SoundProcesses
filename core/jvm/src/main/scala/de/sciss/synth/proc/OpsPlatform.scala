package de.sciss.synth.proc

import de.sciss.lucre.Txn

trait OpsPlatform {
  implicit def audioCueObjOps[T <: Txn[T]](obj: AudioCue.Obj[T]): AudioCue.Obj.Ops[T] = new AudioCue.Obj.Ops(obj)
}
