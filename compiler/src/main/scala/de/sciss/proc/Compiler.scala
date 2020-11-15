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
