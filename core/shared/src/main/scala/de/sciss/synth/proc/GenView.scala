/*
 *  GenView.scala
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

import de.sciss.lucre.{Disposable, Obj, Observable, Txn, synth}
import de.sciss.synth.proc.impl.{GenViewImpl => Impl}

import scala.util.Try

object GenView {
  /* The current state a view is in. */
  sealed trait State {
    def isComplete: Boolean
  }
//  case object Stopped extends State {
//    def isComplete = false
//  }
  case object Completed extends State {
    def isComplete = true
  }
  case class Running(progress: Double) extends State {
    def isComplete = false
  }

  // ---- factories ----

  trait Factory {
    def typeId: Int

    type Repr[~ <: Txn[~]] <: Obj[~]

    def apply[T <: synth.Txn[T]](obj: Repr[T])(implicit tx: T, universe: Universe[T]): GenView[T]
  }

  def addFactory   (f: Factory): Unit     = Impl.addFactory   (f)
  def tryAddFactory(f: Factory): Boolean  = Impl.tryAddFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[T <: synth.Txn[T]](obj: Obj[T])(implicit tx: T, universe: Universe[T]): GenView[T] = Impl(obj)
}
trait GenView[T <: Txn[T]] extends Observable[T, GenView.State] with Disposable[T] {
  def typeId: Int

  def reactNow(fun: T => GenView.State => Unit)(implicit tx: T): Disposable[T]

  def state(implicit tx: T): GenView.State

  def valueType: Obj.Type

  def value(implicit tx: T): Option[Try[Obj[T]]]
}