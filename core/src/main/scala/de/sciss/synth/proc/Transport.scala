/*
 *  Transport.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.event.Observable
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.synth.proc.impl.{TransportImpl => Impl}

object Transport {
  /** Creates a `Transport` independent of a running aural system. If will create and destroy
    * an aural context with the state of the provided system.
    */
  def apply[S <: SSys[S]](universe: Universe[S])(implicit tx: S#Tx): Transport[S] =
    Impl(universe)

//  /** Creates a `Transport` independent of a running aural system. If will create and destroy
//    * an aural context with the state of the provided system. It creates a new scheduler.
//    */
//  def apply[S <: SSys[S]](aural: AuralSystem)
//                        (implicit tx: S#Tx, cursor: Cursor[S], workspace: WorkspaceHandle[S]): Transport[S] = {
//    val sched = Scheduler[S]
//    apply(aural, sched)
//  }

  /** Creates a `Transport` for a running existing aural context. */
  def apply[S <: SSys[S]](context: AuralContext[S])(implicit tx: S#Tx): Transport[S] =
    Impl[S](context)

  sealed trait Update[S <: Sys[S]] {
    def transport: Transport[S]
  }

  final case class AuralStarted[S <: Sys[S]](transport: Transport[S], context: AuralContext[S]) extends Update[S]

  sealed trait StateUpdate[S <: Sys[S]] extends Update[S] {
    def position: Long
  }

  sealed trait ModelUpdate[S <: Sys[S]] extends Update[S] {
    def obj: Obj[S]
  }

  final case class ObjectAdded  [S <: Sys[S]](transport: Transport[S], obj: Obj[S]) extends ModelUpdate[S]
  final case class ObjectRemoved[S <: Sys[S]](transport: Transport[S], obj: Obj[S]) extends ModelUpdate[S]

  sealed trait ViewUpdate[S <: Sys[S]] extends Update[S] {
    def view: AuralObj[S]
  }

  final case class ViewAdded  [S <: Sys[S]](transport: Transport[S], view: AuralObj[S])
    extends ViewUpdate[S]

  final case class ViewRemoved[S <: Sys[S]](transport: Transport[S], view: AuralObj[S])
    extends ViewUpdate[S]

  final case class Play[S <: Sys[S]](transport: Transport[S], position: Long) extends StateUpdate[S]
  final case class Stop[S <: Sys[S]](transport: Transport[S], position: Long) extends StateUpdate[S]
  final case class Seek[S <: Sys[S]](transport: Transport[S], position: Long, isPlaying: Boolean) extends StateUpdate[S]
}

/** New reduced definition of a t_(P) transport mechanism. */
trait Transport[S <: Sys[S]]
  extends Disposable[S#Tx] with Observable[S#Tx, Transport.Update[S]] {

  def play()(implicit tx: S#Tx): Unit
  def stop()(implicit tx: S#Tx): Unit

  def seek(position: Long)(implicit tx: S#Tx): Unit
  def position(implicit tx: S#Tx): Long

  def isPlaying(implicit tx: S#Tx): Boolean

  def views(implicit tx: S#Tx): Set[AuralObj[S]]

  def getView    (obj: Obj[S])(implicit tx: S#Tx): Option[AuralObj[S]]
  def getViewById(id : S#Id  )(implicit tx: S#Tx): Option[AuralObj[S]]

  def addObject   (obj: Obj[S])(implicit tx: S#Tx): Unit
  def removeObject(obj: Obj[S])(implicit tx: S#Tx): Unit

  // not sure if the transport should generate the context or if use site should provide it?
  def contextOption(implicit tx: S#Tx): Option[AuralContext[S]]

  implicit val universe: Universe[S]

  def scheduler: Scheduler[S]
}