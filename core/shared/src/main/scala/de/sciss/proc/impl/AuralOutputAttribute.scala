/*
 *  AuralOutputAttribute.scala
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

package de.sciss.proc
package impl

import de.sciss.lucre.Txn
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.Source
import de.sciss.lucre.Obj
import de.sciss.lucre.Disposable
import de.sciss.proc.AuralAttribute.{Factory, Observer, Target}
import de.sciss.proc.Runner.{Running, Stopped, Prepared}

import scala.concurrent.stm.Ref

object AuralOutputAttribute extends Factory {
  type Repr[T <: Txn[T]] = Proc.Output[T]

  def tpe: Obj.Type = Proc.Output

  def apply[T <: Txn[T]](key: String, value: Proc.Output[T], observer: Observer[T])
                        (implicit tx: T, context: AuralContext[T]): AuralAttribute[T] =
    new AuralOutputAttribute(key, tx.newHandle(value), observer).init(value)
}
final class AuralOutputAttribute[T <: Txn[T]](val key: String, objH: Source[T, Proc.Output[T]],
                                              observer: Observer[T])
                                             (implicit context: AuralContext[T])
  extends AuralAttributeImpl[T] { attr =>

  override def toString = s"AuralOutputAttribute($key)@${hashCode.toHexString}"

  def tpe: Obj.Type = Proc.Output

  type Repr = Proc.Output[T]

  def obj(implicit tx: T): Proc.Output[T] = objH()

  private[this] val auralRef  = Ref(Option.empty[AuralOutput[T]])
  private[this] var obs: Disposable[T] = _
  private[this] val playRef   = Ref(Option.empty[Target[T]])
  private[this] val aObsRef   = Ref(Option.empty[Disposable[T]])

  def targetOption(implicit tx: T): Option[Target[T]] = playRef()

  def preferredNumChannels(implicit tx: T): Int =
    auralRef().fold(-1)(_.bus.numChannels)

  def init(output: Proc.Output[T])(implicit tx: T): this.type = {
    val id  = output.id // idH()
    obs = context.observeAux[AuralOutput[T]](id) { implicit tx => {
      case AuxContext.Added(_, auralOutput) =>
        auralSeen(auralOutput)
        playRef().foreach(update(_, auralOutput))
        observer.attrNumChannelsChanged(this)
      case AuxContext.Removed(_) =>
        stopNoFire()
    }}
    context.getAux[AuralOutput[T]](id).foreach(auralSeen)
    this
  }

  private def auralSeen(auralOutput: AuralOutput[T])(implicit tx: T): Unit = {
    auralRef() = Some(auralOutput)
    val aObs = auralOutput.react { implicit tx => {
      case AuralOutput.Play(_) =>
        playRef().foreach(update(_, auralOutput))
      case AuralOutput.Stop =>
        // println(s"Aural stopped + ${playRef().isDefined}")
        stopNoFire()
    }}
    aObsRef.swap(Some(aObs)).foreach(_.dispose())
    playRef().foreach(update(_, auralOutput))
  }

  def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit =
    state = Prepared

  def run(timeRef: TimeRef.Option, target: Target[T])(implicit tx: T): Unit /* Instance */ = {
    // println(s"PLAY $this")
    require (playRef.swap(Some(target)).isEmpty)
    // target.add(this)
    auralRef().foreach(update(target, _))
    state = Running
  }

  def stop()(implicit tx: T): Unit = {
    // println(s"STOP $this")
    stopNoFire()
    state = Stopped
  }

  private def stopNoFire()(implicit tx: T): Unit =
    playRef.swap(None).foreach { target =>
      target.remove(this)
    }

  private def update(target: Target[T], audioOutput: AuralOutput[T])(implicit tx: T): Unit = {
    val nodeRefOpt = audioOutput.view.nodeOption
    nodeRefOpt.foreach { nodeRef =>
      target.put(this, AuralAttribute.Stream(nodeRef, audioOutput.bus))
    }
  }

  def dispose()(implicit tx: T): Unit = {
    // println(s"DISPOSE $this")
    stopNoFire()
    auralRef.set(None)
    aObsRef.swap(None).foreach(_.dispose())
    obs.dispose()
  }
}