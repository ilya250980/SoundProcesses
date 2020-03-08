/*
 *  ViewBase.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Disposable, Form, Obj, Sys}

trait ViewBase[S <: Sys[S]] extends Observable[S#Tx, Runner.State] with Disposable[S#Tx] {
  def state(implicit tx: S#Tx): Runner.State

  def stop()(implicit tx: S#Tx): Unit

  /** Like `react`, but also invokes the function with the current state immediately. */
  final def reactNow(fun: S#Tx => Runner.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val res = react(fun)
    fun(tx)(state)
    res
  }
}

trait AuralViewBase[S <: Sys[S], -Target] extends ViewBase[S] {
  type Repr <: Form[S]

  def obj(implicit tx: S#Tx): Repr

  /** Prepares the view to be able to `run`.
    *
    * @param  timeRef   an optional context of temporal position
    */
  def prepare(timeRef: TimeRef.Option)(implicit tx: S#Tx): Unit

  /** Runs the view, whatever that means for the particular object. If the object is not
    * prepared and needs preparing, the view will take care of running the `prepare` step
    * (without mapping any `attr` map).
    */
  def run(timeRef: TimeRef.Option, target: Target)(implicit tx: S#Tx): Unit
}

trait ObjViewBase[S <: Sys[S], -Target] extends AuralViewBase[S, Target] {
  type Repr <: Obj[S]

  def tpe: Obj.Type
}