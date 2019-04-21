/*
 *  OscNode.scala
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

import de.sciss.lucre.expr.ExOps._
import de.sciss.lucre.expr.{Act, Control, Ex, IControl, Trig}
import de.sciss.lucre.stm.Sys

//object OscNode {
//  final val UDP = "udp"
//  final val TCP = "tcp"
//
//  def apply(port: Ex[Int] = -1, transport: Ex[String] = UDP): OscNode = ???
//}
sealed trait OscNode extends Control {
  type Repr[S <: Sys[S]] = IControl[S]

//  def port      : Ex[Int]
//  def transport : Ex[String]

  def self: Ex[SocketAddress]

  def received: Trig

  def packet: Ex[OscPacket]

  def sender: Ex[SocketAddress]

  /** Possible values: 0 (off), 1 (text), 2 (hex), 3 (both) */
  var dump: Ex[Int]

  /** Possible values: `"1.0"`, `"1.1"` */
  var codec: Ex[String]

  //  protected def mkControl[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): Disposable[S#Tx] = ...
}

object OscUdpNode {
  def apply(localPort: Ex[Int] = -1, localHost: Ex[String] = ???): OscUdpNode = ???
}
trait OscUdpNode extends OscNode {
  def send(target: Ex[SocketAddress], p: Ex[OscPacket]): Act

  def reply(p: Ex[OscPacket]): Act
}

object OscTcpClient {
  def apply(targetPort: Ex[SocketAddress], targetHost: Ex[String] = ???): OscTcpClient = ???
}
trait OscTcpClient extends OscNode {
  def send(p: Ex[OscPacket]): Act

  var localPort: Ex[Int]
}

object OscTcpServer {
  def apply(localPort: Ex[Int] = -1, localHost: Ex[String] = ???): OscTcpClient = ???
}
trait OscTcpServer extends OscNode {
  // def sendAll(p: Ex[OscPacket]): Act

  def connected   : Trig
  def disconnected: Trig

  /** Sends to the most recent sender */
  def reply(p: Ex[OscPacket]): Act
}