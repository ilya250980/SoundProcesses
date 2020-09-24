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

import de.sciss.lucre.{Disposable, Form, Obj, Observable, Txn}

trait ViewBase[T <: Txn[T]] extends Observable[T, Runner.State] with Disposable[T] {
  def state(implicit tx: T): Runner.State

  def stop()(implicit tx: T): Unit

  /** Like `react`, but also invokes the function with the current state immediately. */
  final def reactNow(fun: T => Runner.State => Unit)(implicit tx: T): Disposable[T] = {
    val res = react(fun)
    fun(tx)(state)
    res
  }
}

trait AuralViewBase[T <: Txn[T], -Target] extends ViewBase[T] {
  type Repr <: Form[T]

  def obj(implicit tx: T): Repr

  /** Prepares the view to be able to `run`.
    *
    * @param  timeRef   an optional context of temporal position
    */
  def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit

  /** Runs the view, whatever that means for the particular object. If the object is not
    * prepared and needs preparing, the view will take care of running the `prepare` step
    * (without mapping any `attr` map).
    */
  def run(timeRef: TimeRef.Option, target: Target)(implicit tx: T): Unit
}

trait ObjViewBase[T <: Txn[T], -Target] extends AuralViewBase[T, Target] {
  type Repr <: Obj[T]

  def tpe: Obj.Type
}