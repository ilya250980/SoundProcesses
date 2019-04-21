package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.Ex

import scala.collection.immutable.{Seq => ISeq}

sealed trait OscPacket {

}

object OscMessage {
  def apply(name: Ex[String], args: Any*): Ex[OscMessage] = ???

  implicit class Ops(private val m: Ex[OscMessage]) extends AnyVal {
    def name: Ex[String]    = ???
    def args: Ex[ISeq[Any]] = ???
  }
}
trait OscMessage extends OscPacket {
  def name: String

  def args: ISeq[Any]
}

object OscBundle {
  def apply(time: Long, packets: OscPacket*): Ex[OscBundle] = ???
}
trait OscBundle extends OscPacket {
  def time: Long

  def packets: ISeq[OscPacket]
}