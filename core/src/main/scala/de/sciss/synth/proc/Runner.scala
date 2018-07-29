/*
 *  Runner.scala
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
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.synth.Sys

import scala.language.higherKinds

object Runner {
  sealed trait State
  case object Stopped   extends State
  case object Preparing extends State
  case object Prepared  extends State
  case object Running   extends State

  trait Factory {
    def prefix      : String
    def humanName   : String
    def tpe         : Obj.Type

    /** Whether the factory maintains a singleton instance of a runner that will
      * be returned for multiple `mkRunner` calls (maintaining an internal use count
      * based on `mkRunner` and `dispose`) or not. Most objects will ''not'' have singleton runners.
      */
    def isSingleton : Boolean

    type Repr[~ <: Sys[~]] <: Obj[~]

    def mkRunner[S <: Sys[S]](obj: Repr[S])(implicit tx: S#Tx, workspace: WorkspaceHandle[S],
                                            cursor: stm.Cursor[S]): Runner[S]
  }
}
trait Runner[S <: stm.Sys[S]] extends Observable[S#Tx, Runner.State] {
  def factory: Runner.Factory

  def objH: stm.Source[S#Tx, Obj[S]]

  def stop    ()(implicit tx: S#Tx): Unit
  def prepare ()(implicit tx: S#Tx): Unit
  def run     ()(implicit tx: S#Tx): Unit

  def state(implicit tx:S#Tx): Runner.State

  def reactNow(fun: S#Tx => Runner.State => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

  def messages(implicit tx: S#Tx): Any

  implicit def workspace: WorkspaceHandle[S]
  implicit def cursor   : stm.Cursor[S]

/*
- allow both for a `self` and an `invoker` (`Action.Universe`)
- should we have an `value: Any` as in `Action.Universe`?
- actually, `invoker` and potential `value` should go into `play` and/or `prepare`
- `GenContext` (again for `prepare`)
- realtime `Scheduler`?
- `AuralSystem`?
 */
}
