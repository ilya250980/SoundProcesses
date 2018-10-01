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
import de.sciss.lucre.stm.{Cursor, Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.impl.RunnerUniverseImpl

object Universe {
  def dummy[S <: SSys[S]](implicit tx: S#Tx, cursor: stm.Cursor[S]): Disposable[S] = {
    implicit val system: S = tx.system
    implicit val workspace: Workspace[S] = Workspace.Implicits.dummy
    apply[S]()
  }
  
  trait Disposable[S <: Sys[S]] extends Universe[S] with stm.Disposable[S#Tx]

  def apply[S <: SSys[S]]()(implicit tx: S#Tx, cursor: Cursor[S], workspace: Workspace[S]): Disposable[S] =
    RunnerUniverseImpl[S]()

  def apply[S <: SSys[S]](genContext: GenContext[S], scheduler: Scheduler[S], auralSystem: AuralSystem)
                        (implicit tx: S#Tx, cursor: Cursor[S], workspace: Workspace[S]): Disposable[S] =
    RunnerUniverseImpl[S](genContext, scheduler, auralSystem)
}

/** Common base for `Runner.Handler` and `Action.Universe` */
trait Universe[S <: Sys[S]] {
  def auralSystem: AuralSystem

//  def handler: Runner.Handler[S]

  implicit def workspace    : Workspace [S]
  implicit def cursor       : Cursor    [S]
  implicit def genContext   : GenContext[S]
  implicit val scheduler    : Scheduler [S]

//  def mkTransport()(implicit tx: S#Tx): Transport[S]

  def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]]

  def runners(implicit tx: S#Tx): Iterator[Runner[S]]

  /** Creates a new derived universe with a new aural system and a fresh scheduler.
    */
  def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[S])(implicit tx: S#Tx): Universe[S]
}