/*
 *  GenView.scala
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

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.synth.proc.impl.{GenViewImpl => Impl}

import scala.language.higherKinds
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

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: Sys[S]](obj: Repr[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S]
  }

  def addFactory   (f: Factory): Unit     = Impl.addFactory   (f)
  def tryAddFactory(f: Factory): Boolean  = Impl.tryAddFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = Impl(obj)
}
trait GenView[S <: Sys[S]] extends Observable[S#Tx, GenView.State] with Disposable[S#Tx] {
  def typeId: Int

  def reactNow(fun: S#Tx => GenView.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  def state(implicit tx: S#Tx): GenView.State

  def valueType: Obj.Type

  def value(implicit tx: S#Tx): Option[Try[Obj[S]]]
}