/*
 *  AsyncResource.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.synth.NodeRef
import de.sciss.lucre.{Disposable, Txn}
import de.sciss.processor.Processor

trait AsyncResource[T <: Txn[T]] extends Processor[Any] with Disposable[T] {
  def install(b: NodeRef.Full[T])(implicit tx: T): Unit
}