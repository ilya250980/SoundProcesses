/*
 *  NodeIDAllocatorImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth.impl

import de.sciss.lucre.synth.NodeIDAllocator
import scala.concurrent.stm.{InTxn, Ref}

final class NodeIDAllocatorImpl(user: Int, initTemp: Int) extends NodeIDAllocator {
  private val temp = Ref(initTemp)
  private val mask = user << 26

  def alloc()(implicit tx: InTxn): Int = {
    // `getAndTransform`:
    // "Transforms the value referenced by this `Ref` by applying the function
    // `f`, and returns the previous value."
    val x = temp.getAndTransform { old =>
      val next = old + 1
      if (next < 0x03FFFFFF) next else initTemp
    }
    x | mask
  }
}