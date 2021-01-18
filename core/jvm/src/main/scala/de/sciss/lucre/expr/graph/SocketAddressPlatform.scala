package de.sciss.lucre.expr.graph

import java.net.InetAddress

trait SocketAddressPlatform {
  protected def mkLocalHostName(): String =
    InetAddress.getLocalHost.getHostName
}
