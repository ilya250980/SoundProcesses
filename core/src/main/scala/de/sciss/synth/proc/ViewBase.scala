/*
 *  ViewBase.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */
package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}

trait ViewBase[S <: Sys[S], -Target] extends Observable[S#Tx, Runner.State] with Disposable[S#Tx] {
  def state(implicit tx: S#Tx): Runner.State

  def prepare(timeRef: TimeRef.Option                )(implicit tx: S#Tx): Unit
  def run    (timeRef: TimeRef.Option, target: Target)(implicit tx: S#Tx): Unit

  def stop()(implicit tx: S#Tx): Unit

  def reactNow(fun: S#Tx => Runner.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = {
    val res = react(fun)
    fun(tx)(state)
    res
  }
}

trait ObjViewBase[S <: Sys[S], -Target] extends ViewBase[S, Target] {
  def tpe: Obj.Type

  /** The view must store a handle to its underlying model. */
  def objH: stm.Source[S#Tx, Obj[S]]
}