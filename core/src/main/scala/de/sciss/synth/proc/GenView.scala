/*
 *  GenView.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
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
import impl.{GenViewImpl => Impl}

import scala.language.higherKinds
import scala.util.Try

object GenView {
  /* The current state a view is in. */
  sealed trait State {
    def isComplete: Boolean
  }
  case object Stopped extends State {
    def isComplete = false
  }
  case object Completed extends State {
    def isComplete = true
  }
  case class Running(progress: Double) extends State {
    def isComplete = false
  }

  // ---- factories ----

  trait Factory {
    def typeID: Int

    type Repr[~ <: Sys[~]] <: Obj[~]

    def apply[S <: Sys[S]](obj: Repr[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, context: GenContext[S]): GenView[S] = Impl(obj)
}
trait GenView[S <: Sys[S]] extends Observable[S#Tx, GenView.State] with Disposable[S#Tx] {
  def typeID: Int

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Gen[S]]

  def state(implicit tx: S#Tx): GenView.State

  def valueType: Obj.Type
  def value(implicit tx: S#Tx): Option[Try[Obj[S]]]

  def start()(implicit tx: S#Tx): Unit
  def stop ()(implicit tx: S#Tx): Unit
}