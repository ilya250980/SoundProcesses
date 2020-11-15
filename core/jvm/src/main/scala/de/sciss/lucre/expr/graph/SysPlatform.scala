/*
 *  SysPlatform.scala
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

package de.sciss.lucre.expr.graph

import java.io.{ByteArrayOutputStream, File, IOException}
import java.net.URI

import de.sciss.equal.Implicits._
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.graph.Sys.Process.Peer
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.impl.{DummyObservableImpl, IChangeGeneratorEvent}
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.model.Change
import de.sciss.proc
import de.sciss.proc.Runner.{Done, Failed, Prepared, Running, Stopped}
import de.sciss.proc.impl.BasicRunnerImpl
import de.sciss.proc.{SoundProcesses, Universe}

import scala.concurrent.stm.{Ref, TxnLocal}
import scala.sys.process.{Process => SProcess}
import scala.util.Try

trait SysPlatform {
  protected final class ExpandedExit[T <: Txn[T]](code: IExpr[T, Int]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit = {
      val codeV = code.value
      tx.afterCommit {
        sys.exit(codeV)
      }
    }
  }

  private final class Run(val id: Int, val cmd: String, val args: Seq[String], val dir: Option[URI]) {
    def isEmpty : Boolean = cmd.isEmpty
    def nonEmpty: Boolean = !isEmpty
  }

  private val EmptyRun: Run = new Run(-1, "", Nil, None)

  protected final class ExpandedProcess[T <: Txn[T]](cmd: IExpr[T, String], args: IExpr[T, Seq[String]],
                                            dirOpt: Option[IExpr[T, URI]])
                                           (implicit val universe: Universe[T], targets: ITargets[T])
    extends BasicRunnerImpl[T] with Peer[T] { outer =>

    private[this] val idRef     = Ref(0)
    private[this] val runLocal  = TxnLocal(init = EmptyRun, afterCommit = executeRun)
    private[this] val procRef   = Ref(Option.empty[SProcess])

    object output extends IExpr[T, String] with IChangeGeneratorEvent[T, String] {
      private[this] val ref = Ref("")

      def value_=(now: String)(implicit tx: T): Unit = {
        val before  = ref.swap(now)
        if (now != before) {
          fire(Change(before, now))
        }
      }

      def value(implicit tx: T): String = {
        ref()
      }

      def dispose()(implicit tx: T): Unit = ()

      override def changed: IChangeEvent[T, String] = this

      protected def targets: ITargets[T] = outer.targets

      private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): String =
        pull.resolveExpr(this)
    }

    object progress extends proc.Runner.Progress[T] with DummyObservableImpl[T] {
      def current(implicit tx: T): Double = -1
    }

    def prepare(attr: proc.Runner.Attr[T])(implicit tx: T): Unit = {
      state = Prepared
    }

    private def executeRun(r: Run): Unit = {
      procRef.single.swap(None).foreach(_.destroy())
      if (r.nonEmpty) {
        val os      = new ByteArrayOutputStream()
        val dirOpt  = r.dir.flatMap(uri => Try(new File(uri)).toOption)
        val pb      = SProcess(r.cmd +: r.args, dirOpt).#>(os)
        val process = pb.run()
        procRef.single.swap(Some(process)).foreach(_.destroy())
        val t = new Thread(() => {
          val code    = process.exitValue()
          os.flush()
          val outputV = os.toString("UTF-8")
          SoundProcesses.step[T]("Sys.Process result") { implicit tx =>
            output.value_=(outputV)
            if (state === Running && idRef() == r.id) {
              state = if (code == 0) {
                Done
              } else {
                Failed(new IOException(s"Process '${r.cmd}' failed with code $code"))
              }
            }
          }
        }, r.cmd)
        t.start()
      }
    }

    def run()(implicit tx: T): Unit = {
      val cmdV    = cmd .value
      val argsV   = args.value
      val dirV    = dirOpt.flatMap { ex => val v = ex.value; if (v.getPath.isEmpty) None else Some(v) }
      val id      = idRef.transformAndGet(_ + 1)
      val r       = new Run(id = id, cmd = cmdV, args = argsV, dir = dirV)
      runLocal()  = r
      state       = Running
    }

    def stop()(implicit tx: T): Unit = {
      killProcess()
      state = Stopped
    }

    protected def disposeData()(implicit tx: T): Unit =
      killProcess()

    private def killProcess()(implicit tx: T): Unit =
      runLocal() = EmptyRun
  }
}
