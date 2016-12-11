/*
 *  DummySerializerFactory.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.stm
import de.sciss.serial.{DataOutput, DataInput, Serializer}

object DummySerializerFactory {
  def apply[I <: stm.Sys[I]]: DummySerializerFactory[I] = anySer.asInstanceOf[DummySerializerFactory[I]]

  private val anySer = new Impl[stm.InMemory, Nothing]

  private class Impl[I <: stm.Sys[I], A]
    extends Serializer[I#Tx, I#Acc, A] with DummySerializerFactory[I] {

    implicit def dummySerializer[A1]: Serializer[I#Tx, I#Acc, A1] =
      this.asInstanceOf[Serializer[I#Tx, I#Acc, A1]]

    def write(v: A, out: DataOutput): Unit = ()

    def read(in: DataInput, access: I#Acc)(implicit tx: I#Tx): A =
      sys.error("Operation not supported")
  }
}

trait DummySerializerFactory[I <: stm.Sys[I]] {
  implicit def dummySerializer[A]: Serializer[I#Tx, I#Acc, A]
}