package de.sciss.synth
package proc
package graph

import scala.language.implicitConversions

object Ops {
  /** Allows the construction of attribute controls, for example via `"freq".kr`. */
  implicit def stringToControl(name: String): Attribute.Factory =
    new Attribute.Factory(name)
}
