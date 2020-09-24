package de.sciss.lucre.event

import de.sciss.lucre.Event.Node
import de.sciss.lucre.{Event, Txn}
import de.sciss.synth.UGenSource.Vec

// get access to package private members
object Peek {
  def targets[T <: Txn[T]](in: Node[T])(implicit tx: T): Vec[(Byte, Event[T, Any])] = in.getTargets.children
}
