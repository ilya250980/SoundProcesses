/*
 *  SocketAddress.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.Ex

object SocketAddress {
  def apply(host: Ex[String], port: Ex[Int]): SocketAddress = ???
}
trait SocketAddress extends Ex[(String, Int)] {
  def host: Ex[String]
  def port: Ex[Int]
}
