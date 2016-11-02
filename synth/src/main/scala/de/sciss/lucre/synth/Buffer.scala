/*
 *  Buffer.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth

import java.io.File

import de.sciss.lucre.synth.Resource.TimeStamp
import de.sciss.synth.{Buffer => SBuffer}
import de.sciss.synth.io.{AudioFileType, SampleFormat}
import de.sciss.lucre.synth.impl.BufferImpl
import de.sciss.osc
import de.sciss.synth.message.BufferGen

import scala.concurrent.ExecutionContext

object Buffer {
  private var cueBufSz = 32768
  private var recBufSz = 32768

  def defaultCueBufferSize: Int = cueBufSz
  def defaultCueBufferSize_=(value: Int): Unit = {
    validateCueBufferSize(64, value)
    cueBufSz = value
  }

  def defaultRecBufferSize: Int = recBufSz
  def defaultRecBufferSize_=(value: Int): Unit = {
    validateCueBufferSize(64, value)
    recBufSz = value
  }

  private def isPowerOfTwo(value: Int) = (value & (value - 1)) == 0

  private def validateCueBufferSize(server: Server, value: Int): Unit =
    validateCueBufferSize(server.config.blockSize, value)

  private def validateCueBufferSize(minSize: Int, value: Int): Unit = {
    require(isPowerOfTwo(value) && value >= minSize && value <= 131072,
      s"Must be a power of two and in ($minSize, 131072): $value")
  }

  private trait ProxyResource extends Resource with Proxy {
    def self: Buffer

    def isOnline(implicit tx: Txn): Boolean = self.isOnline

    private[synth] def timeStamp                    (implicit tx: Txn): TimeStamp = self.timeStamp
    private[synth] def timeStamp_=(value: TimeStamp)(implicit tx: Txn): Unit      = self.timeStamp = value

    def server: Server = self.server
  }

  /** Utility resource creation that defers buffer disposal until point where node has been freed. */
  def disposeWithNode(buf: Buffer, nr: NodeRef): Resource = new ProxyResource {
    val self: Buffer = buf

    def dispose()(implicit tx: Txn): Unit = nr.node.onEndTxn { implicit tx =>
      // println(s"disposeWithNode($buf, $nr)")
      self.dispose()
    }
  }

  /** Writes the buffer content to a file when the node ends.
    * The caller is responsible for eventually freeing the buffer!
    * This is because we do not open a transaction after writing completes.
    */
  def writeWithNode(buf: Buffer, nr: NodeRef, artifact: File)(action: => Unit)
                   (implicit exec: ExecutionContext): Resource = new ProxyResource {
    val self: Buffer = buf

    def dispose()(implicit tx: Txn): Unit = {
      nr.node.onEnd {
        val fut = self.server.!!(osc.Bundle.now(self.peer.writeMsg(path = artifact.getPath)))
        fut.foreach { _ =>
          action
        }
      }
    }
  }

  def diskIn(server: Server)(path: String, startFrame: Long = 0L, numFrames: Int = defaultCueBufferSize,
                             numChannels: Int = 1)(implicit tx: Txn): Buffer = {
    validateCueBufferSize(server, numFrames)
    val res = create(server, numFrames = numFrames, numChannels = numChannels, closeOnDisposal = true)
    // res.allocRead(path, startFrame = startFrame, numFrames = numFrames)
    res.alloc()
    res.cue(path, fileStartFrame = startFrame)
    res
  }

  def diskOut(server: Server)(path: String, fileType: AudioFileType = AudioFileType.AIFF,
                              sampleFormat: SampleFormat = SampleFormat.Float,
                              numFrames: Int = defaultRecBufferSize, numChannels: Int = 1)(implicit tx: Txn): Buffer = {
    validateCueBufferSize(server, numFrames)
    val res = create(server, numFrames = numFrames, numChannels = numChannels, closeOnDisposal = true)
    res.alloc()
    res.record(path, fileType, sampleFormat)
    res
  }

  def fft(server: Server)(size: Int)(implicit tx: Txn): Modifiable = {
    require(size >= 2 && isPowerOfTwo(size), "Must be a power of two and >= 2 : " + size)
    val res = create(server, numFrames = size, numChannels = 1)
    res.alloc()
    res
  }

  def apply(server: Server)(numFrames: Int, numChannels: Int = 1)(implicit tx: Txn): Modifiable = {
    val res = create(server, numFrames = numFrames, numChannels = numChannels)
    res.alloc()
    res
  }

  private def create(server: Server, numFrames: Int, numChannels: Int, closeOnDisposal: Boolean = false)
                    (implicit tx: Txn): BufferImpl = {
    val id    = server.allocBuffer()
    val peer  = SBuffer(server.peer, id)
    BufferImpl(server, peer)(numFrames = numFrames, numChannels = numChannels, closeOnDisposal = closeOnDisposal)
  }

  trait Modifiable extends Buffer {
    def zero()(implicit tx: Txn): Unit

    def fill(index: Int, num: Int, value: Float)(implicit tx: Txn): Unit

    def gen(cmd: BufferGen.Command)(implicit tx: Txn): Unit

    def read(path: String, fileStartFrame: Long = 0L, numFrames: Int = -1, bufStartFrame: Int = 0)
            (implicit tx: Txn): Unit

    def readChannel(path: String, channels: Seq[Int], fileStartFrame: Long = 0L, numFrames: Int = -1,
                    bufStartFrame: Int = 0)(implicit tx: Txn): Unit
  }
}

trait Buffer extends Resource {
  def peer: SBuffer
  def id: Int

  def numChannels: Int
  def numFrames  : Int

  def write(path: String, fileType: AudioFileType = AudioFileType.AIFF,
            sampleFormat: SampleFormat = SampleFormat.Float, numFrames: Int = -1, startFrame: Int = 0,
            leaveOpen: Boolean = false /* , completion: Optional[Packet] = None */)(implicit tx: Txn): Unit
}