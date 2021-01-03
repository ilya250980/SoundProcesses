/*
 *  Transport.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc

import de.sciss.lucre.expr.Context
import de.sciss.lucre.{Disposable, Ident, Obj, Observable, Txn, synth}
import de.sciss.proc.impl.{TransportImpl => Impl}

object Transport {
  /** Creates a `Transport` independent of a running aural system, along with attributes.
    * It will create and destroy an aural context with the state of the provided system.
    */
  def apply[T <: synth.Txn[T]](universe: Universe[T], attr: Context.Attr[T])(implicit tx: T): Transport[T] =
    Impl(universe, attr)

  /** Creates a `Transport` independent of a running aural system.
    * It will create and destroy an aural context with the state of the provided system.
    */
  def apply[T <: synth.Txn[T]](universe: Universe[T])(implicit tx: T): Transport[T] =
    Impl(universe)

  /** Creates a `Transport` for a running existing aural context, along with attributes. */
  def apply[T <: synth.Txn[T]](context: AuralContext[T], attr: Context.Attr[T])(implicit tx: T): Transport[T] =
    Impl[T](context, attr)

  /** Creates a `Transport` for a running existing aural context. */
  def apply[T <: synth.Txn[T]](context: AuralContext[T])(implicit tx: T): Transport[T] =
    Impl[T](context)

  sealed trait Update[T <: Txn[T]] {
    def transport: Transport[T]
  }

  final case class AuralStarted[T <: Txn[T]](transport: Transport[T], context: AuralContext[T]) extends Update[T]

  sealed trait StateUpdate[T <: Txn[T]] extends Update[T] {
    def position: Long
  }

  sealed trait ModelUpdate[T <: Txn[T]] extends Update[T] {
    def obj: Obj[T]
  }

  final case class ObjectAdded  [T <: Txn[T]](transport: Transport[T], obj: Obj[T]) extends ModelUpdate[T]
  final case class ObjectRemoved[T <: Txn[T]](transport: Transport[T], obj: Obj[T]) extends ModelUpdate[T]

  sealed trait ViewUpdate[T <: Txn[T]] extends Update[T] {
    def view: AuralObj[T]
  }

  final case class ViewAdded  [T <: Txn[T]](transport: Transport[T], view: AuralObj[T])
    extends ViewUpdate[T]

  final case class ViewRemoved[T <: Txn[T]](transport: Transport[T], view: AuralObj[T])
    extends ViewUpdate[T]

  final case class Play[T <: Txn[T]](transport: Transport[T], position: Long) extends StateUpdate[T]
  final case class Stop[T <: Txn[T]](transport: Transport[T], position: Long) extends StateUpdate[T]
  final case class Seek[T <: Txn[T]](transport: Transport[T], position: Long, isPlaying: Boolean) extends StateUpdate[T]
}

/** New reduced definition of a t_(P) transport mechanism. */
trait Transport[T <: Txn[T]]
  extends Disposable[T] with Observable[T, Transport.Update[T]] {

  def play()(implicit tx: T): Unit
  def stop()(implicit tx: T): Unit

  def seek(position: Long)(implicit tx: T): Unit
  def position(implicit tx: T): Long

  def isPlaying(implicit tx: T): Boolean

  def views(implicit tx: T): Set[AuralObj[T]]

  def getView    (obj: Obj  [T])(implicit tx: T): Option[AuralObj[T]]
  def getViewById(id : Ident[T])(implicit tx: T): Option[AuralObj[T]]

  def addObject   (obj: Obj[T])(implicit tx: T): Unit
  def removeObject(obj: Obj[T])(implicit tx: T): Unit

  // not sure if the transport should generate the context or if use site should provide it?
  def contextOption(implicit tx: T): Option[AuralContext[T]]

  implicit val universe: Universe[T]

  def scheduler: Scheduler[T]
}