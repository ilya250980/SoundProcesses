/*
 *  BufferImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.synth
package impl

import de.sciss.synth.{FillRange, Buffer => SBuffer}
import de.sciss.synth.io.{AudioFileType, SampleFormat}
import de.sciss.synth.message.BufferGen

final case class BufferImpl(server: Server, peer: SBuffer)
                           (val numFrames: Int, val numChannels: Int, closeOnDisposal: Boolean)
  extends ResourceImpl with Buffer.Modifiable {

  def id: Int = peer.id

  def alloc()(implicit tx: Txn): Unit = {
    requireOffline()
    if (numFrames   < 0) throw new IllegalArgumentException(s"numFrames ($numFrames) must be >= 0")
    if (numChannels < 0) throw new IllegalArgumentException(s"numChannels ($numChannels) must be >= 0")
    tx.addMessage(this, peer.allocMsg(numFrames = numFrames, numChannels = numChannels))
    setOnline(value = true)
  }

  /** Allocates and reads the buffer content once (closes the file). */
  def allocRead(path: String, startFrame: Long)(implicit tx: Txn): Unit = {
    requireOffline()
    if (startFrame > 0x7FFFFFFFL) throw new IllegalArgumentException(s"Cannot encode start frame >32 bit ($startFrame)")
    // require(numFrames >= 0, s"numFrames ($numFrames) must be >= 0")
    val frameI = startFrame.toInt
    tx.addMessage(this, peer.allocReadMsg(path, startFrame = frameI, numFrames = numFrames))
    setOnline(value = true)
  }

  /** Opens a file to write to in streaming mode (leaving it open), as usable for DiskOut. */
  def record(path: String, fileType: AudioFileType = AudioFileType.AIFF,
             sampleFormat: SampleFormat = SampleFormat.Float)(implicit tx: Txn): Unit = {
    write(path, fileType, sampleFormat, numFrames = 0, leaveOpen = true)
  }

  /** Writes the buffer contents once (closes the target file). */
  def write(path: String, fileType: AudioFileType, sampleFormat: SampleFormat, numFrames: Int,
            startFrame: Int, leaveOpen: Boolean)(implicit tx: Txn): Unit = {
    requireOnline()
    if (leaveOpen != closeOnDisposal) throw new IllegalArgumentException(s"leaveOpen is $leaveOpen but should be $closeOnDisposal")
    if (startFrame < 0) throw new IllegalArgumentException(s"startFrame ($startFrame) must be >= 0")
    tx.addMessage(this, peer.writeMsg(path, fileType, sampleFormat, numFrames = numFrames, startFrame = startFrame,
      leaveOpen = leaveOpen))
  }

  /** Cues the input sound file for streaming via DiskIn (leaves the file open). */
  def cue(path: String, fileStartFrame: Long = 0L, numFrames: Int = -1)(implicit tx: Txn): Unit = {
    requireOnline()
    if (fileStartFrame > 0x7FFFFFFFL) throw new IllegalArgumentException(s"Cannot encode start frame >32 bit ($fileStartFrame)")
    // if (numFrames < 0) throw new IllegalArgumentException(s"numFrames ($numFrames) must be >= -1")
    val frameI = fileStartFrame.toInt
    tx.addMessage(this, peer.readMsg(path, fileStartFrame = frameI, numFrames = numFrames, leaveOpen = true))
  }

  /** Reads the buffer contents from a file (closes the file). */
  def read(path: String, fileStartFrame: Long = 0L, numFrames: Int = -1, bufStartFrame: Int = 0)(implicit tx: Txn): Unit = {
    requireOnline()
    if (fileStartFrame > 0x7FFFFFFFL) throw new IllegalArgumentException(s"Cannot encode start frame >32 bit ($fileStartFrame)")
    if (bufStartFrame < 0) throw new IllegalArgumentException(s"bufStartFrame ($bufStartFrame) must be >= 0")
    val frameI = fileStartFrame.toInt
    tx.addMessage(this, peer.readMsg(path, fileStartFrame = frameI, bufStartFrame = bufStartFrame,
      numFrames = numFrames, leaveOpen = false))
  }

  // XXX TODO - DRY with `read`
  def readChannel(path: String, channels: Seq[Int],
                  fileStartFrame: Long = 0L, numFrames: Int = -1, bufStartFrame: Int = 0)(implicit tx: Txn): Unit = {
    requireOnline()
    if (fileStartFrame > 0x7FFFFFFFL) throw new IllegalArgumentException(s"Cannot encode start frame >32 bit ($fileStartFrame)")
    if (bufStartFrame < 0) throw new IllegalArgumentException(s"bufStartFrame ($bufStartFrame) must be >= 0")
    val frameI = fileStartFrame.toInt
    tx.addMessage(this, peer.readChannelMsg(path, fileStartFrame = frameI, bufStartFrame = bufStartFrame,
      numFrames = numFrames, leaveOpen = false, channels = channels))
  }

  /** Clears the buffer contents. */
  def zero()(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.zeroMsg)
  }

  /** Clears the buffer contents. */
  def fill(index: Int, num: Int, value: Float)(implicit tx: Txn): Unit = {
    val size = numFrames * numChannels
    if (index < 0         ) throw new IllegalArgumentException (s"index ($index) must be >= 0")
    if (num   < 0         ) throw new IllegalArgumentException (s"num ($num) must be >= 0")
    if (index + num > size) throw new IndexOutOfBoundsException(s"index ($index) + num ($num) > size ($size)")
    requireOnline()
    tx.addMessage(this, peer.fillMsg(FillRange(index = index, num = num, value = value)))
  }

  def setn(values: IndexedSeq[Float])(implicit tx: Txn): Unit = setn((0, values))

  def setn(pairs: (Int, IndexedSeq[Float])*)(implicit tx: Txn): Unit = {
    val size = numFrames * numChannels
    pairs.foreach { case (index, values) =>
      val num = values.size
      if (index < 0         ) throw new IllegalArgumentException (s"index ($index) must be >= 0")
      if (index + num > size) throw new IndexOutOfBoundsException(s"index ($index) + num ($num) > size ($size)")
    }
    requireOnline()
    tx.addMessage(this, peer.setnMsg(pairs: _*))
  }

  def gen(cmd: BufferGen.Command)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.genMsg(cmd))
  }

  def sine1(partials: Seq[Float], normalize: Boolean = true, wavetable: Boolean = true, clear: Boolean = true)
          (implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.sine1Msg(
      partials = partials, normalize = normalize, wavetable = wavetable, clear = clear))
  }

  def sine2(partials: Seq[(Float, Float)], normalize: Boolean = true, wavetable: Boolean = true,
            clear: Boolean = true)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.sine2Msg(
      partials = partials, normalize = normalize, wavetable = wavetable, clear = clear))
  }

  def sine3(partials: Seq[(Float, Float, Float)], normalize: Boolean = true, wavetable: Boolean = true,
            clear: Boolean = true)(implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.sine3Msg(
      partials = partials, normalize = normalize, wavetable = wavetable, clear = clear))
  }

  def cheby(amps: Seq[Float], normalize: Boolean = true, wavetable: Boolean = true, clear: Boolean = true)
           (implicit tx: Txn): Unit = {
    requireOnline()
    tx.addMessage(this, peer.chebyMsg(
      amps = amps, normalize = normalize, wavetable = wavetable, clear = clear))
  }

  def dispose()(implicit tx: Txn): Unit = {
    requireOnline()
    if (closeOnDisposal) {
      tx.addMessage(this, peer.closeMsg)
    }
    tx.addMessage(this, peer.freeMsg(release = false))
    server.freeBuffer(peer.id)
    setOnline(value = false)
  }
}