package de.sciss.proc

import de.sciss.lucre.synth.Txn

/** This object should compile and thereby confirm
  * that serializer resolution works for elements and objects.
  */
object TypeInferenceAndImplicitResolution {
  def test[T <: Txn[T]]()(implicit tx: T): Unit = {
    val graph: AudioCue.Obj[T] = sys.error("Not necessary for compilation")
    val peer  = graph // AudioGraphemeElem(graph)
    val obj   = peer // Obj(peer)

    val ph    = tx.newHandle(peer)
    val oh    = tx.newHandle(obj)

    val pr: AudioCue.Obj[T] /* AudioGraphemeElem[S] */ = ph()
    val or: AudioCue.Obj[T] /* Obj.T[S, AudioGraphemeElem] */ = oh()

    println(s"If this compiles, $pr and $or are fine.")
  }
}
