/*
 *  Sys.scala
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

import java.io.{ByteArrayOutputStream, IOException}

import de.sciss.equal.Implicits._
import de.sciss.file.File
import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, Graph, IAction}
import de.sciss.lucre.impl.{DummyObservableImpl, IChangeGeneratorEvent}
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn, synth}
import de.sciss.model.Change
import de.sciss.synth.proc
import de.sciss.synth.proc.Runner.{Done, Failed, Prepared, Running, Stopped}
import de.sciss.synth.proc.impl.BasicRunnerImpl
import de.sciss.synth.proc.{SoundProcesses, Universe}

import scala.concurrent.stm.{Ref, TxnLocal}
import scala.sys.process.{Process => SProcess}

/** Access to operating system functions. */
object Sys {
  private final class ExpandedProperty[T <: Txn[T]](key: IExpr[T, String], tx0: T)
                                                   (implicit targets: ITargets[T])
    extends MappedIExpr[T, String, Option[String]](key, tx0) {

    protected def mapValue(inValue: String)(implicit tx: T): Option[String] =
      sys.props.get(inValue)
  }

  /** A system property. */
  final case class Property(key: Ex[String]) extends Ex[Option[String]] {
    override def productPrefix: String = s"Sys$$Property" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[String]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ExpandedProperty[T](key.expand[T], tx)
    }
  }

  private final class ExpandedExit[T <: Txn[T]](code: IExpr[T, Int]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit = {
      val codeV = code.value
      tx.afterCommit {
        sys.exit(codeV)
      }
    }
  }

  final case class Exit(code: Ex[Int] = 0) extends Act {
    override def productPrefix: String = s"Sys$$Exit" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedExit[T](code.expand[T])
  }

  private final class ExpandedEnv[T <: Txn[T]](key: IExpr[T, String], tx0: T)
                                                   (implicit targets: ITargets[T])
    extends MappedIExpr[T, String, Option[String]](key, tx0) {

    protected def mapValue(inValue: String)(implicit tx: T): Option[String] =
      sys.env.get(inValue)
  }

  /** An environment variable. */
  final case class Env(key: Ex[String]) extends Ex[Option[String]] {
    override def productPrefix: String = s"Sys$$Env" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[String]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ExpandedEnv[T](key.expand[T], tx)
    }
  }

  /** A shell process. */
  object Process {

    // XXX TODO: this overload doesn't resolve nicely
//    /** Creates a new shell process for a given command and arguments.
//      * To run the process, use the `run` action. Observe the termination
//      * through `done` or `failed`.
//      */
//    def apply(cmd: Ex[String])(args: Ex[String]*): Process = apply(cmd, args)

    /** Creates a new shell process for a given command and arguments.
      * To run the process, use the `run` action. Observe the termination
      * through `done` or `failed`.
      */
    def apply(cmd: Ex[String], args: Ex[Seq[String]] = Nil): Process = Impl(cmd, args)

    private final val keyDirectory = "directory"

    final case class Directory(p: Process) extends Ex[File] {
      type Repr[T <: Txn[T]] = IExpr[T, File]

      override def productPrefix: String = s"Sys$$Process$$Directory" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val valueOpt = ctx.getProperty[Ex[File]](p, keyDirectory)
        valueOpt.fold(Const(new File("")).expand[T])(_.expand[T])
      }
    }

    final case class Output(p: Process) extends Ex[String] {
      type Repr[T <: Txn[T]] = IExpr[T, String]

      override def productPrefix: String = s"Sys$$Process$$Output" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val px = p.expand[T]
        px.output
      }
    }

    private val EmptyRun: Run = new Run(-1, "", Nil, None)

    private final class Run(val id: Int, val cmd: String, val args: Seq[String], val dir: Option[File]) {
      def isEmpty : Boolean = cmd.isEmpty
      def nonEmpty: Boolean = !isEmpty
    }

    private final class Expanded[T <: Txn[T]](cmd: IExpr[T, String], args: IExpr[T, Seq[String]],
                                              dirOpt: Option[IExpr[T, File]])
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
          val os  = new ByteArrayOutputStream()
          val pb  = SProcess(r.cmd +: r.args, r.dir).#>(os)
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

    private final case class Impl(cmd: Ex[String], args: Ex[Seq[String]]) extends Process {

      override def productPrefix: String = s"Sys$$Process" // serialization

      type Repr[T <: Txn[T]] = Peer[T]

      def directory: Ex[File] = Process.Directory(this)

      def directory_=(value: Ex[File]): Unit = {
        val b = Graph.builder
        b.putProperty(this, keyDirectory, value)
      }

      def output: Ex[String] = Process.Output(this)

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
        tx match {
          case stx: synth.Txn[_] =>
            // XXX TODO --- ugly ugly ugly
            mkControlImpl[stx.Ev](ctx.asInstanceOf[Context[stx.Ev]], tx.asInstanceOf[stx.Ev])
              .asInstanceOf[Repr[T]]

          case _ => throw new Exception("Need a SoundProcesses system")
        }

      private def mkControlImpl[T <: synth.Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.{cursor, targets, workspace}
        implicit val h: Universe[T] = Universe()
        val dirOpt = ctx.getProperty[Ex[File]](this, keyDirectory).map(_.expand[T])
        new Expanded[T](cmd.expand[T], args.expand[T], dirOpt)
      }
    }

    trait Peer[T <: Txn[T]] extends proc.Runner[T] {
      def output: IExpr[T, String]
    }
  }

  /** A shell process. */
  trait Process extends Runner {
    type Repr[T <: Txn[T]] <: Process.Peer[T]

    /** The process' current working directory. */
    var directory: Ex[File]

    def output: Ex[String]

    // var environment: Ex[Map[String, String]]
  }
}
