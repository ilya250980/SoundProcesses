/*
 *  BufferPrepare.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.proc.impl

import java.util.concurrent.TimeUnit

import de.sciss.audiofile.AudioFileSpec
import de.sciss.lucre.synth.{Buffer, Executor, NodeRef, RT}
import de.sciss.lucre.{Artifact, Txn, synth}
import de.sciss.osc
import de.sciss.processor.impl.ProcessorBase
import de.sciss.synth.proc.graph

import scala.concurrent.stm.Ref
import scala.concurrent.stm.TxnExecutor.{defaultAtomic => atomic}
import scala.concurrent.{Future, TimeoutException}

object BufferPrepare {

  /** The configuration of the buffer preparation.
    *
    * @param f        the audio file to read in
    * @param spec     the file's specification (number of channels and frames)
    * @param offset   the offset into the file to start with
    * @param buf      the buffer to read into. This buffer must have been allocated already.
    * @param key      the key of the `graph.Buffer` element, used for setting the synth control eventually
    */
  case class Config(f: Artifact.Value, spec: AudioFileSpec, offset: Long, buf: Buffer.Modifiable, key: String) {
    override def productPrefix = "BufferPrepare.Config"
    override def toString = {
      import spec.{productPrefix => _, _}
      s"$productPrefix($f, numChannels = $numChannels, numFrames = $numFrames, offset = $offset, key = $key)"
    }
  }

  /** Creates and launches the process. */
  def apply[T <: synth.Txn[T]](config: Config)(implicit tx: T): AsyncResource[T] = {
    import config._
    if (!buf.isOnline) sys.error("Buffer must be allocated")
    val numFrL = spec.numFrames
    if (numFrL > 0x3FFFFFFF) sys.error(s"File $f is too large ($numFrL frames) for an in-memory buffer")
    val res = new Impl[T](path = f.getPath, numFrames = numFrL.toInt, off0 = offset,
      numChannels = spec.numChannels, buf = buf, key = key)
    import Executor.executionContext
    tx.afterCommit(res.start())
    res
  }

  private final class Impl[T <: synth.Txn[T]](path: String, numFrames: Int, off0: Long, numChannels: Int,
                                        buf: Buffer.Modifiable, key: String)
    extends AsyncResource[T] with ProcessorBase[Any, AsyncResource[T]] { self =>

    type Prod = Any

    private val blockSize = 262144 / numChannels  // XXX TODO - could be configurable
    private val offsetRef = Ref(0)

    override def toString = s"BufferPrepare($path, $buf)@${hashCode().toHexString}"

    // ---- processor body ----

    protected def runBody(): Future[Prod] = {
      // make sure we always have a `Future` and avoid throwing an exception from `body` directly
      def loop(): Future[Unit] =
        if (progress == 1.0) Future.successful(() /*buf*/) else Future.unit.flatMap { _ =>
          val pr = atomic { implicit tx =>
            val offset = offsetRef()
            val chunk = math.min(numFrames - offset, blockSize)
            val stop = offset + chunk
            if (chunk > 0) {
              offsetRef() = stop
              implicit val ptx: RT = RT.wrap(tx)
              // buf might be offline if dispose was called
              if (buf.isOnline) {
                buf.read(path, fileStartFrame = offset + off0, numFrames = chunk, bufStartFrame = offset)
                if (stop == numFrames) 1.0 else math.min(0.9999, stop.toDouble / numFrames)
              } else -1.0
            } else -1.0
          }

          if (pr < 0) abort()
          checkAborted()

          val fut = buf.server.!!(osc.Bundle.now()) // aka 'sync', so we let other stuff be processed first
          if (fut.isCompleted) {
            progress = pr
            loop()
          } else {
            val futTimeOut = Executor.timeOut(fut, 1L, TimeUnit.SECONDS).recover {
              case _: TimeoutException => ()
            }
            futTimeOut.flatMap { _ =>
              checkAborted()
              loop()
            }
          }
        }

      loop()
    }

    // ----

    private[this] val installed = Ref(false)

    def install(b: NodeRef.Full[T])(implicit tx: T): Unit = {
      import Txn.peer
      require(!installed.swap(true))
      val ctlName = graph.Buffer.controlName(key)
      b.addControl(ctlName -> buf.id)
      val late = Buffer.disposeWithNode(buf, b)
      b.addResource(late)
    }

    def dispose()(implicit tx: T): Unit = {
      import Txn.peer
      tx.afterCommit(abort())
      if (buf.isOnline && !installed()) buf.dispose()
    }
  }
}
// trait BufferPrepare extends Processor[Buffer.Modifiable, BufferPrepare] with Disposable[Txn]
