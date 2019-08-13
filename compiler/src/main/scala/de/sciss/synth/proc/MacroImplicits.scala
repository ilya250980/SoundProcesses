/*
 *  MacroImplicits.scala
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

import de.sciss.lucre
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.impl.Macros

import scala.language.experimental.macros

/** Enables implicits extensions
  * to assign various graph programs (`SynthGraph`, `Control.Graph`, etc.) to
  * their containing objects (`Proc`, `Control`, etc.)
  * from a standard IDE,
  * compiling these objects correctly for storage in the workspace,
  * and preserving the corresponding source code.
  */
object MacroImplicits {
  implicit final class ProcMacroOps[S <: Sys[S]](val `this`: Proc[S]) extends AnyVal {
    def setGraph(body: Unit)(implicit tx: S#Tx): Unit =
      macro Macros.procGraphWithSource[S]
  }

  implicit final class ActionMacroOps(private val a: Action.type) extends AnyVal {
    def apply[S <: Sys[S]](body: Action.Universe[S] => Unit)(implicit tx: S#Tx): Action[S] =
    macro Macros.actionWithSource[S]
  }

  implicit final class ControlMacroOps[S <: Sys[S]](val `this`: Control[S]) extends AnyVal {
    def setGraph(body: Unit)(implicit tx: S#Tx): Unit =
    macro Macros.controlGraphWithSource[S]
  }

  implicit final class WidgetMacroOps[S <: Sys[S]](val `this`: Widget[S]) extends AnyVal {
    def setGraph(body: lucre.swing.graph.Widget)(implicit tx: S#Tx): Unit =
      macro Macros.widgetGraphWithSource[S]
  }
}