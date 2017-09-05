/*
 *  BufferPrepare.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import java.io.File

import de.sciss.lucre.stm.TxnLike
import de.sciss.lucre.synth.{Buffer, NodeRef, Sys, Txn}
import de.sciss.osc
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.synth.io.AudioFileSpec

import scala.concurrent.{Await, TimeoutException, blocking, duration}
import duration.Duration
import scala.concurrent.stm.{Ref, TxnExecutor}
import TxnExecutor.{defaultAtomic => atomic}

object BufferPrepare {

  /** The configuration of the buffer preparation.
    *
    * @param f        the audio file to read in
    * @param spec     the file's specification (number of channels and frames)
    * @param offset   the offset into the file to start with
    * @param buf      the buffer to read into. This buffer must have been allocated already.
    * @param key      the key of the `graph.Buffer` element, used for setting the synth control eventually
    */
  case class Config(f: File, spec: AudioFileSpec, offset: Long, buf: Buffer.Modifiable, key: String) {
    override def productPrefix = "BufferPrepare.Config"
    override def toString = s"$productPrefix($f, numChannels = ${spec.numChannels}, numFrames = ${spec.numFrames}, offset = $offset, key = $key)"
  }

  /** Creates and launches the process. */
  def apply[S <: Sys[S]](config: Config)(implicit tx: S#Tx): AsyncResource[S] = {
    import config._
    if (!buf.isOnline) sys.error("Buffer must be allocated")
    val numFrL = spec.numFrames
    if (numFrL > 0x3FFFFFFF) sys.error(s"File $f is too large ($numFrL frames) for an in-memory buffer")
    val res = new Impl[S](path = f.getAbsolutePath, numFrames = numFrL.toInt, off0 = offset,
      numChannels = spec.numChannels, buf = buf, key = key)
    import SoundProcesses.executionContext
    tx.afterCommit(res.start())
    res
  }

  private final class Impl[S <: Sys[S]](path: String, numFrames: Int, off0: Long, numChannels: Int,
                                        buf: Buffer.Modifiable, key: String)
    extends AsyncResource[S] with ProcessorImpl[Any, AsyncResource[S]] {

    private val blockSize = 262144 / numChannels  // XXX TODO - could be configurable
    private val offsetRef = Ref(0)

    override def toString = s"BufferPrepare($path, $buf)@${hashCode().toHexString}"

    protected def body(): Buffer.Modifiable = {
      while (progress < 1.0) {
        val pr = atomic { implicit tx =>
          val offset = offsetRef()
          val chunk = math.min(numFrames - offset, blockSize)
          val stop = offset + chunk
          if (chunk > 0) {
            offsetRef() = stop
            implicit val ptx: Txn = Txn.wrap(tx)
            // buf might be offline if dispose was called
            if (buf.isOnline) {
              buf.read(path, fileStartFrame = offset + off0, numFrames = chunk, bufStartFrame = offset)
              stop.toDouble / numFrames
            } else -1.0
          } else -1.0
        }

        if (pr < 0) abort()
        checkAborted()

        val fut = buf.server.!!(osc.Bundle.now()) // aka 'sync', so we let other stuff be processed first
        while (!fut.isCompleted) {
          blocking {
            try {
              // we wait max. 1 second, so we can cross-check on the aborted status
              Await.result(fut, Duration(1L, duration.SECONDS))
            } catch {
              case _: TimeoutException =>
            }
          }
          checkAborted()
        }
        progress = pr
      }
      buf
    }

    private[this] val installed = Ref(false)

    def install(b: NodeRef.Full[S])(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      require(!installed.swap(true))
      val ctlName = graph.Buffer.controlName(key)
      b.addControl(ctlName -> buf.id)
      val late = Buffer.disposeWithNode(buf, b)
      b.addResource(late)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      import TxnLike.peer
      tx.afterCommit(abort())
      if (buf.isOnline && !installed()) buf.dispose()
    }
  }
}
// trait BufferPrepare extends Processor[Buffer.Modifiable, BufferPrepare] with Disposable[Txn]
