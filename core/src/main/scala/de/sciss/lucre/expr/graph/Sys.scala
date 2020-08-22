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
import de.sciss.lucre.event.impl.{DummyObservableImpl, IChangeGenerator}
import de.sciss.lucre.event.{IChangeEvent, IPull, ITargets}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, Graph, IAction, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth
import de.sciss.model.Change
import de.sciss.synth.proc
import de.sciss.synth.proc.Runner.{Done, Failed, Prepared, Running, Stopped}
import de.sciss.synth.proc.impl.BasicRunnerImpl
import de.sciss.synth.proc.{SoundProcesses, Universe}

import scala.concurrent.stm.{Ref, TxnLocal}
import scala.sys.process.{Process => SProcess}

/** Access to operating system functions. */
object Sys {
  private final class ExpandedProperty[S <: Sys[S]](key: IExpr[S, String], tx0: S#Tx)
                                                   (implicit targets: ITargets[S])
    extends MappedIExpr[S, String, Option[String]](key, tx0) {

    protected def mapValue(inValue: String)(implicit tx: S#Tx): Option[String] =
      sys.props.get(inValue)
  }

  /** A system property. */
  final case class Property(key: Ex[String]) extends Ex[Option[String]] {
    override def productPrefix: String = s"Sys$$Property" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[String]]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ExpandedProperty[S](key.expand[S], tx)
    }
  }

  private final class ExpandedExit[S <: Sys[S]](code: IExpr[S, Int]) extends IActionImpl[S] {
    def executeAction()(implicit tx: S#Tx): Unit = {
      val codeV = code.value
      tx.afterCommit {
        sys.exit(codeV)
      }
    }
  }

  final case class Exit(code: Ex[Int] = 0) extends Act {
    override def productPrefix: String = s"Sys$$Exit" // serialization

    type Repr[S <: Sys[S]] = IAction[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      new ExpandedExit[S](code.expand[S])
  }

  private final class ExpandedEnv[S <: Sys[S]](key: IExpr[S, String], tx0: S#Tx)
                                                   (implicit targets: ITargets[S])
    extends MappedIExpr[S, String, Option[String]](key, tx0) {

    protected def mapValue(inValue: String)(implicit tx: S#Tx): Option[String] =
      sys.env.get(inValue)
  }

  /** An environment variable. */
  final case class Env(key: Ex[String]) extends Ex[Option[String]] {
    override def productPrefix: String = s"Sys$$Env" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Option[String]]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ExpandedEnv[S](key.expand[S], tx)
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
      type Repr[S <: Sys[S]] = IExpr[S, File]

      override def productPrefix: String = s"Sys$$Process$$Directory" // serialization

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        val valueOpt = ctx.getProperty[Ex[File]](p, keyDirectory)
        valueOpt.fold(Const(new File("")).expand[S])(_.expand[S])
      }
    }

    final case class Output(p: Process) extends Ex[String] {
      type Repr[S <: Sys[S]] = IExpr[S, String]

      override def productPrefix: String = s"Sys$$Process$$Output" // serialization

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        val px = p.expand[S]
        px.output
      }
    }

    private val EmptyRun: Run = new Run(-1, "", Nil, None)

    private final class Run(val id: Int, val cmd: String, val args: Seq[String], val dir: Option[File]) {
      def isEmpty : Boolean = cmd.isEmpty
      def nonEmpty: Boolean = !isEmpty
    }

    private final class Expanded[S <: Sys[S]](cmd: IExpr[S, String], args: IExpr[S, Seq[String]],
                                              dirOpt: Option[IExpr[S, File]])
                                             (implicit val universe: Universe[S], targets: ITargets[S])
      extends BasicRunnerImpl[S] with Peer[S] { outer =>

      private[this] val idRef     = Ref(0)
      private[this] val runLocal  = TxnLocal(init = EmptyRun, afterCommit = executeRun)
      private[this] val procRef   = Ref(Option.empty[SProcess])

      object output extends IExpr[S, String] with IChangeGenerator[S, String] {
        private[this] val ref = Ref("")

        def value_=(now: String)(implicit tx: S#Tx): Unit = {
          val before  = ref.swap(now)
          if (now != before) {
            fire(Change(before, now))
          }
        }

        def value(implicit tx: S#Tx): String = {
          ref()
        }

        def dispose()(implicit tx: S#Tx): Unit = ()

        override def changed: IChangeEvent[S, String] = this

        protected def targets: ITargets[S] = outer.targets

        private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): String =
          pull.resolveExpr(this)
      }

      object progress extends proc.Runner.Progress[S#Tx] with DummyObservableImpl[S] {
        def current(implicit tx: S#Tx): Double = -1
      }

      def prepare(attr: proc.Runner.Attr[S])(implicit tx: S#Tx): Unit = {
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
            SoundProcesses.step[S]("Sys.Process result") { implicit tx =>
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

      def run()(implicit tx: S#Tx): Unit = {
        val cmdV    = cmd .value
        val argsV   = args.value
        val dirV    = dirOpt.flatMap { ex => val v = ex.value; if (v.getPath.isEmpty) None else Some(v) }
        val id      = idRef.transformAndGet(_ + 1)
        val r       = new Run(id = id, cmd = cmdV, args = argsV, dir = dirV)
        runLocal()  = r
        state       = Running
      }

      def stop()(implicit tx: S#Tx): Unit = {
        killProcess()
        state = Stopped
      }

      protected def disposeData()(implicit tx: S#Tx): Unit =
        killProcess()

      private def killProcess()(implicit tx: S#Tx): Unit =
        runLocal() = EmptyRun
    }

    private final case class Impl(cmd: Ex[String], args: Ex[Seq[String]]) extends Process {

      override def productPrefix: String = s"Sys$$Process" // serialization

      type Repr[S <: Sys[S]] = Peer[S]

      def directory: Ex[File] = Process.Directory(this)

      def directory_=(value: Ex[File]): Unit = {
        val b = Graph.builder
        b.putProperty(this, keyDirectory, value)
      }

      def output: Ex[String] = Process.Output(this)

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
        tx.system match {
          case _: synth.Sys[_] =>
            // XXX TODO --- ugly ugly ugly
            mkControlImpl[synth.NoSys](ctx.asInstanceOf[Context[synth.NoSys]], tx.asInstanceOf[synth.NoSys#Tx])
              .asInstanceOf[Repr[S]]

          case _ => throw new Exception("Need a SoundProcesses system")
        }

      private def mkControlImpl[S <: synth.Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.{cursor, targets, workspace}
        implicit val h: Universe[S] = Universe()
        val dirOpt = ctx.getProperty[Ex[File]](this, keyDirectory).map(_.expand[S])
        new Expanded[S](cmd.expand[S], args.expand[S], dirOpt)
      }
    }

    trait Peer[S <: Sys[S]] extends proc.Runner[S] {
      def output: IExpr[S, String]
    }
  }

  /** A shell process. */
  trait Process extends Runner {
    type Repr[S <: Sys[S]] <: Process.Peer[S]

    /** The process' current working directory. */
    var directory: Ex[File]

    def output: Ex[String]

    // var environment: Ex[Map[String, String]]
  }
}
