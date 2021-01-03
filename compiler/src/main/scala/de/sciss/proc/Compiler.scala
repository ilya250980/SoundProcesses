/*
 *  Compiler.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.proc.impl.CompilerImpl

object Compiler {
  /** Creates a new compiler. Note that the
   * peer `IMain` is lazily initialized, so
   * if you spawn compilation in a future,
   * you effectively get an asynchronous initialization.
   */
  def apply(): Code.Compiler = CompilerImpl()
}
