/*
 *  MacroCompilerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc.impl

import java.io.File

import scala.tools.nsc.interpreter.IMain
import scala.tools.nsc.interpreter.shell.ReplReporterImpl
import scala.tools.nsc.{ConsoleWriter, Global, NewLinePrintWriter}

object MacroCompilerImpl {
  def apply(peer: Global): IMain = {
    // this is the trick to get the right class-path -- we steal it from the macro compiler
    val cSet = peer.settings.copy()
    cSet.warnUnused.clear()
    cSet.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val writer = new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)
    val reporter = new ReplReporterImpl(cSet, writer)
    new IMain(cSet, reporter)
  }
}
