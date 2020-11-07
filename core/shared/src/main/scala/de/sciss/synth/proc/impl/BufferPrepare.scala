/*
 *  BufferPrepare.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import java.util.concurrent.TimeUnit

import de.sciss.audiofile.AudioFileSpec
import de.sciss.lucre.synth.{Buffer, Executor, NodeRef, RT}
import de.sciss.lucre.{Artifact, Txn, synth}
import de.sciss.model.impl.ModelImpl
import de.sciss.osc
import de.sciss.processor.Processor
import de.sciss.processor.impl.FutureProxy
import de.sciss.synth.proc.graph

import scala.concurrent.stm.Ref
import scala.concurrent.stm.TxnExecutor.{defaultAtomic => atomic}
import scala.concurrent.{ExecutionContext, Future, Promise, TimeoutException}

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
    import Executor.context
    tx.afterCommit(res.start())
    res
  }

  private final class Impl[T <: synth.Txn[T]](path: String, numFrames: Int, off0: Long, numChannels: Int,
                                        buf: Buffer.Modifiable, key: String)
    extends AsyncResource[T]
//      with ProcessorImpl[Any, AsyncResource[T]]
      with Processor.Prepared
      with Processor.Body
      with ModelImpl[Processor.Update[Any, AsyncResource[T]]]
      with FutureProxy[Any]
  { self =>

    type Prod = Any

    // ---- begin async proc ----

    private var _context: ExecutionContext = _
    @volatile private var _aborted    = false

    @volatile private var _progress   = 0.0
    @volatile private var _lastProg   = -1 // per mille resolution

    private val promise = Promise[Prod]()

    protected def peerFuture: Future[Any] = promise.future

    /** Keeps a record of the execution context used for starting this processor.
     * You may use this to start intermediate sub processes. This method may only
     * be used in the `body` method.
     */
    final implicit protected def executionContext: ExecutionContext = {
      if (_context == null) throw new IllegalStateException("Called before the processor was started")
      _context
    }

    /** Subclasses may override this to be informed immediately. about an abort request.
     * Otherwise they can pull the aborted status any time by invoking `checkAborted()`.
     */
    protected def notifyAborted(): Unit = ()

    final def abort(): Unit = promise.synchronized {
      if (!_aborted) {
        _aborted = true
//        if (_child != null) _child.abort()
        notifyAborted()
      }
    }

    /** The resolution at which progress reports are dispatched. The default of `100` means that
     * a `Processor.Progress` message is only dispatched if the progress has advanced by at least 1 percent.
     * Higher values give finer granularity (sub classes may override this value).
     */
    protected val progressResolution = 100

    /** Invoke this to signalize progress
     *
     * @param f the processor's progress in percent (0 to 1). Values outside the 0 to 1 range will be clipped.
     */
    final def progress_=(f: Double): Unit = {
      val f0    = if (f < 0.0) 0.0 else if (f > 1.0) 1.0 else f
      _progress = f0
      val i     = (f0 * progressResolution).toInt
      if (i > _lastProg) {
        _lastProg = i
        dispatch(Processor.Progress(self, f0))
      }
    }

    final def progress: Double = _progress

    /** Subclasses may override this to perform further cleanup when the process is aborted. */
    protected def cleanUp(): Unit = ()

    /** Checks if the process was aborted. If so, throws an `Aborted` exception. The main body
     * should _not_ try to catch this exception, which will be handled by the underlying infrastructure.
     * However, the main body should put resource operations in proper `try ... finally` blocks, so
     * that these resources are freed when `Abort` exception is thrown. Alternatively, the `cleanUp`
     * method can be overridden to perform such tasks.
     */
    final def checkAborted(): Unit = if (_aborted) throw Processor.Aborted()

    /** Returns `true` if the `abort` method had been called. */
    final def aborted: Boolean = _aborted

    // ---- end async proc ----

    private val blockSize = 262144 / numChannels  // XXX TODO - could be configurable
    private val offsetRef = Ref(0)

    override def toString = s"BufferPrepare($path, $buf)@${hashCode().toHexString}"

    // ---- processor body ----

    final def start()(implicit executionContext: ExecutionContext): Unit = promise.synchronized {
      if (_context != null) return  // was already started
      _context = executionContext
      val res = body().andThen { case _ => cleanUp() }
      res.onComplete(t => dispatch(Processor.Result(self, t)))
      promise.completeWith(res)
      ()
    }

    protected def body(): Future[Prod] = {
      // make sure we always have a `Future` and avoid throwing an exception from `body` directly
      if (progress == 1.0) Future.successful(buf) else Future.unit.flatMap { _ =>
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

        def await(): Future[Prod] =
          if (fut.isCompleted) {
            progress = pr
            body()
          } else {
            val futTimeOut = Executor.timeOut(fut, 1L, TimeUnit.SECONDS).recover {
              case _: TimeoutException => ()
            }
            futTimeOut.flatMap { _ =>
              checkAborted()
              await()
            }
          }

        await()
      }
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
