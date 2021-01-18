package de.sciss.lucre.expr.graph

trait SocketAddressPlatform {
  protected def mkLocalHostName(): String = "localhost"
}