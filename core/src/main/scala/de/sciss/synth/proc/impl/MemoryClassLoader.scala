/*
 *  MemoryClassLoader.scala
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
package impl

import scala.concurrent.stm.{InTxn, TMap, TSet}

final class MemoryClassLoader extends ClassLoader {
  private[this] val setAdded    = TSet.empty[String]
  private[this] val mapClasses  = TMap.empty[String, Array[Byte]]
  private[this] val DEBUG       = false

  def add(name: String, jar: Array[Byte])(implicit tx: InTxn): Unit = {
    val isNew = setAdded.add(name)
    if (DEBUG) println(s"ActionImpl: Class loader add '$name' - isNew? $isNew")
    if (isNew) {
      val entries = Code.unpackJar(jar)
      if (DEBUG) {
        entries.foreach { case (n, _) =>
          println(s"...'$n'")
        }
      }
      mapClasses ++= entries
    }
  }

  override protected def findClass(name: String): Class[_] =
    mapClasses.single.get(name).map { bytes =>
      if (DEBUG) println(s"ActionImpl: Class loader: defineClass '$name'")
      defineClass(name, bytes, 0, bytes.length)

    } .getOrElse {
      if (DEBUG) println(s"ActionImpl: Class loader: not found '$name' - calling super")
      super.findClass(name) // throws exception
    }
}
