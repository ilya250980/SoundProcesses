/*
 *  MacroCompilerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.{ConsoleWriter, Global, NewLinePrintWriter}

object MacroCompilerImpl {
  def apply(peer: Global): IMain = {
    // this is the trick to get the right class-path -- we steal it from the macro compiler
    val cSet       = peer.settings.copy()
    val writer    = new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)
    new IMain(cSet, writer)
  }
}