/*
 *  Ops.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package proc
package graph

import scala.language.implicitConversions

object Ops {
  /** Allows the construction of attribute controls, for example via `"freq".kr`. */
  implicit def stringToControl(name: String): Attribute.Factory =
    new Attribute.Factory(name)
}
