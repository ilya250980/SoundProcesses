package de.sciss.lucre.synth
package expr

import de.sciss.lucre.{event => evt}
import evt.Targets
import de.sciss.synth.Curve
import de.sciss.serial.{DataOutput, DataInput}

object Curves extends BiTypeImpl[Curve] {
  final val typeID = 15

  def readValue(in: DataInput): Curve = Curve.serializer.read(in)

  def writeValue(value: Curve, out: DataOutput): Unit = Curve.serializer.write(value, out)

  // ---- protected ----

  def readTuple[S <: evt.Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                            (implicit tx: S#Tx): ExN[S] = {
    cookie match {
      case _ => sys.error("Invalid cookie " + cookie)
    }
  }
}
