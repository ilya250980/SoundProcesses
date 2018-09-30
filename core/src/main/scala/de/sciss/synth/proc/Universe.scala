/*
 *  Universe.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Disposable, Obj, Sys, WorkspaceHandle}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.impl.RunnerHandlerImpl

object Universe {
  def dummy[S <: SSys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Universe[S] with Disposable[S#Tx] = {
    implicit val workspace: WorkspaceHandle[S] = WorkspaceHandle.Implicits.dummy
    apply[S]()
  }

  def apply[S <: SSys[S]]()(implicit tx: S#Tx, cursor: Cursor[S],
                            workspace: WorkspaceHandle[S]): Universe[S] with Disposable[S#Tx] =
    RunnerHandlerImpl[S]()

  def apply[S <: SSys[S]](genContext: GenContext[S], scheduler: Scheduler[S], auralSystem: AuralSystem)
                        (implicit tx: S#Tx, cursor: Cursor[S],
                         workspace: WorkspaceHandle[S]): Universe[S] with Disposable[S#Tx] =
    RunnerHandlerImpl[S](genContext, scheduler, auralSystem)
}

/** Common base for `Runner.Handler` and `Action.Universe` */
trait Universe[S <: Sys[S]] {
  def auralSystem: AuralSystem

//  def handler: Runner.Handler[S]

  implicit def workspace    : WorkspaceHandle [S]
  implicit def cursor       : Cursor          [S]
  implicit def genContext   : GenContext      [S]
  implicit val scheduler    : Scheduler       [S]

//  def mkTransport()(implicit tx: S#Tx): Transport[S]

  def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]]

  def runners(implicit tx: S#Tx): Iterator[Runner[S]]

  /** Creates a new derived universe with a new aural system and a fresh scheduler.
    */
  def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[S]): Universe[S]
}