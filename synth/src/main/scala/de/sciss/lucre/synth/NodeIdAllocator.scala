/*
 *  NodeIdAllocator.scala
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

package de.sciss.lucre.synth

import impl.{NodeIdAllocatorImpl => Impl}
import scala.concurrent.stm.InTxn

object NodeIdAllocator {
  def apply(user: Int, initTemp: Int): NodeIdAllocator = new Impl(user = user, initTemp = initTemp)
}
trait NodeIdAllocator{
  def alloc()(implicit tx: InTxn): Int
}