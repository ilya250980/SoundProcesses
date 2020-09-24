/*
 *  MacroImplicits.scala
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

package de.sciss.synth.proc

import de.sciss.lucre
import de.sciss.lucre.Txn
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
  implicit final class ProcMacroOps[T <: Txn[T]](val `this`: Proc[T]) extends AnyVal {
    def setGraph(body: Unit)(implicit tx: T): Unit =
      macro Macros.procGraphWithSource[T]
  }

//  implicit final class ActionRawMacroOps(private val a: ActionRaw.type) extends AnyVal {
//    def apply[T <: Txn[T]](body: Action.Universe[T] => Unit)(implicit tx: T): ActionRaw[T] =
//      macro Macros.actionRawWithSource[T]
//  }

  implicit final class ControlMacroOps[T <: Txn[T]](val `this`: Control[T]) extends AnyVal {
    def setGraph(body: Unit)(implicit tx: T): Unit =
      macro Macros.controlGraphWithSource[T]
  }

  implicit final class ActionMacroOps[T <: Txn[T]](val `this`: Action[T]) extends AnyVal {
    def setGraph(body: lucre.expr.graph.Act)(implicit tx: T): Unit =
      macro Macros.actionGraphWithSource[T]
  }
  implicit final class WidgetMacroOps[T <: Txn[T]](val `this`: Widget[T]) extends AnyVal {
    def setGraph(body: lucre.swing.graph.Widget)(implicit tx: T): Unit =
        macro Macros.widgetGraphWithSource[T]
  }
}