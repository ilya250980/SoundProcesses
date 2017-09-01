/*
 *  CompilerImplicits.scala
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

package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.impl.Macros

import scala.language.experimental.macros

/** Enables implicits extensions
  * to assign `SynthGraph`s to a `Proc` and to create
  * instances of `Action` from a standard IDE,
  * compiling these objects correctly for storage in the workspace,
  * and preserving the corresponding source code.
  */
object CompilerImplicits {
  implicit final class ProcCompilerOps[S <: Sys[S]](private[proc] val `this`: Proc[S]) extends AnyVal {
    def setGraphWithSource(body: Unit)(implicit tx: S#Tx): Unit =
      macro Macros.procGraphWithSource
  }
}