/*
 *  Universe.scala
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
import de.sciss.lucre.stm.{Cursor, Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.impl.RunnerUniverseImpl

object Universe {
  def dummy[S <: SSys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Universe[S] = {
    implicit val system: S = tx.system
    implicit val workspace: Workspace[S] = Workspace.Implicits.dummy
    apply[S]()
  }

  def apply[S <: SSys[S]]()(implicit tx: S#Tx, cursor: Cursor[S], workspace: Workspace[S]): Universe[S] =
    RunnerUniverseImpl[S]()

  def apply[S <: SSys[S]](genContext: GenContext[S], scheduler: Scheduler[S], auralSystem: AuralSystem)
                        (implicit tx: S#Tx, cursor: Cursor[S], workspace: Workspace[S]): Universe[S] =
    RunnerUniverseImpl[S](genContext, scheduler, auralSystem)

  sealed trait Update[S <: Sys[S]]
  final case class Added  [S <: Sys[S]](r: Runner[S]) extends Update[S]
  final case class Removed[S <: Sys[S]](r: Runner[S]) extends Update[S]

  /** Common base for `Universe` and `Action.Universe` */
  trait Base[S <: Sys[S]] {
    def auralSystem: AuralSystem

    implicit def workspace    : Workspace [S]
    implicit def cursor       : Cursor    [S]
    implicit def genContext   : GenContext[S]
    implicit val scheduler    : Scheduler [S]
  }
}
trait Universe[S <: Sys[S]] extends Universe.Base[S] with stm.Disposable[S#Tx] with Observable[S#Tx, Universe.Update[S]]{
  def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]]

  def runners(implicit tx: S#Tx): Iterator[Runner[S]]

  private[proc] def removeRunner(r: Runner[S])(implicit tx: S#Tx): Unit

  /** Creates a new derived universe with a new aural system and a fresh scheduler.
    */
  def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[S])(implicit tx: S#Tx): Universe[S]
}