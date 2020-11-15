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

import java.net.URI

import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.expr.{Context, Graph, IAction}
import de.sciss.lucre.{IExpr, ITargets, Txn, synth, Artifact => _Artifact}
import de.sciss.synth.proc
import de.sciss.synth.proc.Universe

/** Access to operating system functions. */
object Sys extends SysPlatform {
  /** A shell process. */
  object Process {
    /** Creates a new shell process for a given command and arguments.
     * To run the process, use the `run` action. Observe the termination
     * through `done` or `failed`.
     */
    def apply(cmd: Ex[String], args: Ex[Seq[String]] = Nil): Process = Impl(cmd, args)

    private final val keyDirectory = "directory"

    final case class Directory(p: Process) extends Ex[URI] {
      type Repr[T <: Txn[T]] = IExpr[T, URI]

      override def productPrefix: String = s"Sys$$Process$$Directory" // serialization

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        val valueOpt = ctx.getProperty[Ex[URI]](p, keyDirectory)
        valueOpt.fold(Const(_Artifact.Value.empty).expand[T])(_.expand[T])
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

    private final case class Impl(cmd: Ex[String], args: Ex[Seq[String]]) extends Process {

      override def productPrefix: String = s"Sys$$Process" // serialization

      type Repr[T <: Txn[T]] = Peer[T]

      def directory: Ex[URI] = Process.Directory(this)

      def directory_=(value: Ex[URI]): Unit = {
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
        val dirOpt = ctx.getProperty[Ex[URI]](this, keyDirectory).map(_.expand[T])
        new ExpandedProcess[T](cmd.expand[T], args.expand[T], dirOpt)
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
    var directory: Ex[URI]

    def output: Ex[String]

    // var environment: Ex[Map[String, String]]
  }

  final case class Exit(code: Ex[Int] = 0) extends Act {
    override def productPrefix: String = s"Sys$$Exit" // serialization

    type Repr[T <: Txn[T]] = IAction[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      new ExpandedExit[T](code.expand[T])
  }

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
}
