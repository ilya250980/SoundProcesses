/*
 *  TxnImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2014 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package impl

import de.sciss.osc
import concurrent.stm.InTxn
import de.sciss.synth.message
import scala.concurrent.stm.{Txn => ScalaTxn}

object TxnImpl {
  var timeoutFun: () => Unit = () => ()

  private final val noBundles = Txn.Bundles(0, Vector.empty)
}

sealed trait TxnImpl extends Txn { tx =>
  import TxnImpl._

  private var bundlesMap = Map.empty[Server, Txn.Bundles]

  final protected def flush(): Unit =
    bundlesMap.foreach { case (server, bundles) =>
      log(s"flush $server -> ${bundles.payload.size} bundles")
      NodeGraph.send(server, bundles)
    }

  protected def markBundlesDirty(): Unit

  final def addMessage(resource: Resource, m: osc.Message with message.Send, dependencies: Seq[Resource]): Unit = {

    val server        = resource.server
    if (!server.peer.isRunning) return

    val resourceStampOld = resource.timeStamp(tx)
    if (resourceStampOld < 0) sys.error(s"Already disposed : $resource")

    implicit val itx  = peer
    val txnCnt        = NodeGraph.messageTimeStamp(server)(tx)
    val txnStopCnt    = txnCnt.get
    val bOld          = bundlesMap.getOrElse(server, noBundles)
    val txnStartCnt   = txnStopCnt - bOld.payload.size

    // calculate the maximum time stamp from the dependencies. this includes
    // the resource as its own dependency (since we should send out messages
    // in monotonic order)
    var depStampMax = math.max(txnStartCnt << 1, resourceStampOld)
    dependencies.foreach { dep =>
      val depStamp = dep.timeStamp(tx)
      if (depStamp < 0) sys.error(s"Dependency already disposed : $dep")
      if (depStamp > depStampMax) depStampMax = depStamp
      // dep.addDependent( resource )( tx )  // validates dependent's server
    }

    // val dAsync     = (dTsMax & 1) == 1
    val msgAsync = !m.isSynchronous

    // if the message is asynchronous, it suffices to ensure that the time stamp async bit is set.
    // otherwise clear the async flag (& ~1), and if the maximum dependency is async, increase the time stamp
    // (from bit 1, i.e. `+ 2`); this second case is efficiently produced through 'rounding up' (`(_ + 1) & ~1`).
    val resourceStampNew = if (msgAsync) depStampMax | 1 else (depStampMax + 1) & ~1

    log(s"addMessage($resource, $m) -> stamp = $resourceStampNew")
    if (resourceStampNew != resourceStampOld) resource.timeStamp_=(resourceStampNew)(tx)

    val bNew = if (bOld.payload.isEmpty) {
      markBundlesDirty()
      //         log( "registering after commit handler" )
      //         afterCommit( flush() )
      val txnStartCntNew = resourceStampNew >> 1
      assert(txnStartCntNew == txnStartCnt)
      txnCnt += 1
      Txn.Bundles(txnStartCntNew, Vector(Vector(m)))

    } else {
      val cntOld = bOld.firstCnt
      val resourceCnt = resourceStampNew >> 1
      val payOld = bOld.payload
      val szOld = payOld.size
      //         if( resourceCnt == cntOld - 1 ) {   // prepend to front
      //            val payNew = Vec( message ) +: payOld
      //            bOld.copy( firstCnt = resourceCnt, payload = payNew )
      //
      //         } else
      if (resourceCnt == cntOld + szOld) {
        // append to back
        val payNew = payOld :+ Vector(m)
        txnCnt += 1
        bOld.copy(payload = payNew)

      } else {
        // we don't need the assertion, since we are going to call payload.apply which would
        // through an out of bounds exception if the assertion wouldn't hold
        //            assert( idxNew >= idxOld && idxNew < idxOld + szOld )
        val payIdx = resourceCnt - cntOld
        val payNew = payOld.updated(payIdx, payOld(payIdx) :+ m)
        bOld.copy(payload = payNew)
      }
    }

    bundlesMap += server -> bNew
  }
}

trait TxnFullImpl[S <: Sys[S]] extends TxnImpl with Sys.Txn[S] {
  final protected def markBundlesDirty(): Unit = {
    log("registering after commit handler")
    afterCommit(flush())
  }
}

final class TxnPlainImpl(val peer: InTxn) extends TxnImpl {
  override def toString = s"proc.Txn<plain>@${hashCode().toHexString}"

  def afterCommit(code: => Unit): Unit = ScalaTxn.afterCommit(_ => code)(peer)

  protected def markBundlesDirty(): Unit = {
    log("registering after commit handler")
    ScalaTxn.afterCommit(_ => flush())(peer)
  }
}