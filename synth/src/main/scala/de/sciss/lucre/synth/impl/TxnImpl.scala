/*
 *  TxnImpl.scala
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

package de.sciss.lucre.synth.impl

import de.sciss.lucre.synth.{Resource, Server, Sys, Txn, log}
import de.sciss.synth.UGenSource.Vec

import scala.concurrent.stm.{InTxn, Ref, Txn => ScalaTxn}

object TxnImpl {
  var timeoutFun: () => Unit = () => ()

  private final val noBundles = Vector.empty: Txn.Bundles
}

sealed trait TxnImpl extends Txn { tx =>
  import TxnImpl._

  // note: they will always be added in pairs (async -> sync),
  // possibly with empty bundles as placeholders
  private var bundlesMap = Map.empty[Server, Txn.Bundles]

  final protected def flush(): Unit =
    bundlesMap.foreach { case (server, bundles) =>
      log(s"flush $server -> ${bundles.size} bundles")
      val ms1 = systemTimeNanoSec / 1000 / 1000
      val ts1 = if (ms1 == 0) "<now>" else de.sciss.osc.TimeTag.millis(ms1).toString
      val ts2 = de.sciss.osc.TimeTag.millis(System.currentTimeMillis()).toString
      println(s":::: flush $ts1 vs $ts2")
      server.send(bundles, systemTimeNanoSec: Long)
    }

  // indicate that we have bundles to flush after the transaction commits
  protected def markBundlesDirty(): Unit

  final def addMessage(resource: Resource, m: Txn.Message, dependencies: Seq[Resource]): Unit = {
    val server        = resource.server
    if (!server.peer.isRunning) return

    val resourceStampOld = resource.timeStamp(tx)
    if (resourceStampOld < 0) sys.error(s"Already disposed : $resource")

    implicit val itx: InTxn     = peer
    val txnStampRef: Ref[Int]   = server.messageTimeStamp
    val txnStamp                = txnStampRef.get
    val payOld: Vec[Txn.Bundle] = bundlesMap.getOrElse(server, noBundles)
    val szOld                   = payOld.size
    val txnStartStamp           = txnStamp - szOld

    // calculate the maximum time stamp from the dependencies. this includes
    // the resource as its own dependency (since we should send out messages
    // in monotonic order)
    var depStampMax = math.max(txnStartStamp, resourceStampOld)
    dependencies.foreach { dep =>
      val depStamp = dep.timeStamp(tx)
      if (depStamp < 0) sys.error(s"Dependency already disposed : $dep")
      if (depStamp > depStampMax) depStampMax = depStamp
      // dep.addDependent(resource)(tx)  // validates dependent's server
    }

    // val dAsync     = (dTsMax & 1) == 1
    val msgAsync = !m.isSynchronous

    // if the message is asynchronous, it suffices to ensure that the time stamp async bit is set.
    // otherwise clear the async flag (& ~1), and if the maximum dependency is async, increase the time stamp
    // (from bit 1, i.e. `+ 2`); this second case is efficiently produced through 'rounding up' (`(_ + 1) & ~1`).
    // val resourceStampNew = if (msgAsync) depStampMax | 1 else (depStampMax + 1) & ~1

    // Actually, we do it reverse; even indices for asynchronous and odd indices for synchronous messages; thus:
    // (A sync  1, B sync  1) --> A | 1
    // (A async 0, B sync  1) --> A | 1
    // (A sync  1, B async 0) --> (A + 1) & ~1 == A + 2
    // (A async 0, B async 0) --> (A + 1) & ~1 == A
    val resourceStampNew = if (msgAsync) (depStampMax + 1) & ~1 else depStampMax | 1

    log(s"addMessage($resource, $m) -> stamp = $resourceStampNew")
    if (resourceStampNew != resourceStampOld) resource.timeStamp_=(resourceStampNew)(tx)

    val bNew = if (szOld == 0) {
      markBundlesDirty()
      txnStampRef += 2
      val vm        = Vector.empty :+ m
      val messages  = if (msgAsync) {
        val b1 = new Txn.Bundle(txnStartStamp     , vm)
        val b2 = new Txn.Bundle(txnStartStamp + 1 , Vector.empty)
        Vector.empty :+ b1 :+ b2
      } else {
        val b1 = new Txn.Bundle(txnStartStamp     , Vector.empty)
        val b2 = new Txn.Bundle(txnStartStamp + 1 , vm)
        Vector.empty :+ b1 :+ b2
      }
      messages: Txn.Bundles

    } else {
      if (resourceStampNew == txnStamp) {
        // append to back
        val vm      = Vector(m)
        val payNew  = if (msgAsync) {
          val b1 = new Txn.Bundle(txnStartStamp     , vm)
          val b2 = new Txn.Bundle(txnStartStamp + 1 , Vector.empty)
          payOld :+ b1 :+ b2
        } else {
          val b1 = new Txn.Bundle(txnStartStamp     , Vector.empty)
          val b2 = new Txn.Bundle(txnStartStamp + 1 , vm)
          payOld :+ b1 :+ b2
        }
        txnStampRef += 2
        payNew: Txn.Bundles

      } else {
        // we don't need the assertion, since we are going to call payload.apply which would
        // throw an out of bounds exception if the assertion wouldn't hold
        //            assert (idxNew >= idxOld && idxNew < idxOld + szOld)
        val payIdx = resourceStampNew - txnStartStamp
        val payNew = payOld.updated(payIdx, payOld(payIdx).append(m))
        payNew: Txn.Bundles
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

final class TxnPlainImpl(val peer: InTxn, val systemTimeNanoSec: Long) extends TxnImpl {
  override def toString = s"proc.Txn<plain>@${hashCode().toHexString}"

  def afterCommit(code: => Unit): Unit = ScalaTxn.afterCommit(_ => code)(peer)

  protected def markBundlesDirty(): Unit = {
    log("registering after commit handler")
    ScalaTxn.afterCommit(_ => flush())(peer)
  }
}