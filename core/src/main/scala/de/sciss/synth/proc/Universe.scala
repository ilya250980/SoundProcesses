/*
 *  Universe.scala
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

import de.sciss.lucre.{Cursor, Disposable, Obj, Observable, Sys, Txn, Workspace, synth}
import de.sciss.synth.proc.impl.RunnerUniverseImpl

object Universe {
  def dummy[T <: synth.Txn[T]](implicit tx: T, cursor: Cursor[T]): Universe[T] = {
    implicit val system: Sys = tx.system
    implicit val workspace: Workspace[T] = Workspace.Implicits.dummy
    apply[T]()
  }

  def apply[T <: synth.Txn[T]]()(implicit tx: T, cursor: Cursor[T], workspace: Workspace[T]): Universe[T] =
    RunnerUniverseImpl[T]()

  def apply[T <: synth.Txn[T]](genContext: GenContext[T], scheduler: Scheduler[T], auralSystem: AuralSystem)
                        (implicit tx: T, cursor: Cursor[T], workspace: Workspace[T]): Universe[T] =
    RunnerUniverseImpl[T](genContext, scheduler, auralSystem)

  sealed trait Update[T <: Txn[T]]
  final case class Added  [T <: Txn[T]](r: Runner[T]) extends Update[T]
  final case class Removed[T <: Txn[T]](r: Runner[T]) extends Update[T]

  /** Common base for `Universe` and `Action.Universe` */
  trait Base[T <: Txn[T]] {
    def auralSystem: AuralSystem

    implicit def workspace    : Workspace [T]
    implicit def cursor       : Cursor    [T]
    implicit def genContext   : GenContext[T]
    implicit val scheduler    : Scheduler [T]
  }
}
trait Universe[T <: Txn[T]] extends Universe.Base[T] with Disposable[T] with Observable[T, Universe.Update[T]] {
  /** Since we obtain `Universe[_]` from some methods, this is lesser evil, since we
    * cannot make totally "wrong" casts here. */
  def cast[T1 <: Txn[T1]]: Universe[T1] = this.asInstanceOf[Universe[T1]]

  def mkRunner(obj: Obj[T])(implicit tx: T): Option[Runner[T]]

  def runners(implicit tx: T): Iterator[Runner[T]]

  private[proc] def removeRunner(r: Runner[T])(implicit tx: T): Unit

  /** Creates a new derived universe with a new aural system and a fresh scheduler.
    */
  def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[T])(implicit tx: T): Universe[T]
}