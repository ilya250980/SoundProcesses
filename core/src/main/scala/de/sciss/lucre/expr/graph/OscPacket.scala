/*
 *  OscPacket.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.lucre.event.IPush.Parents
import de.sciss.lucre.event.impl.{IChangeEventImpl, IGenerator}
import de.sciss.lucre.event.{IChangeEvent, IEvent, IPull, ITargets}
import de.sciss.lucre.expr.{Context, ExSeq, IAction, IExpr, ITrigger}
import de.sciss.lucre.stm.Sys
import de.sciss.synth.UGenSource.Vec

sealed trait OscPacket

object OscMessage {
  def apply(name: Ex[String], args: Ex[Any]*): Ex[OscMessage] =
    Impl(name, args: _*)

  private final class NameExpanded[S <: Sys[S]](peer: IExpr[S, OscMessage], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S])
    extends IExpr[S, String] with IChangeEventImpl[S, String] {

    peer.changed.--->(changed)(tx0)

    def value(implicit tx: S#Tx): String = 
      peer.value.name

    def dispose()(implicit tx: S#Tx): Unit =
      peer.changed.-/->(changed)

    def changed: IChangeEvent[S, String] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): String = {
      val m = pull.expr(peer)
      m.name
    }
  }

  final case class Name(m: Ex[OscMessage]) extends Ex[String] {
    type Repr[S <: Sys[S]] = IExpr[S, String]

    override def productPrefix: String = s"OscMessage$$Name" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new NameExpanded(m.expand[S], tx)
    }
  }

  private final class ArgsExpanded[S <: Sys[S]](peer: IExpr[S, OscMessage], tx0: S#Tx)
                                               (implicit protected val targets: ITargets[S])
    extends IExpr[S, Seq[Any]] with IChangeEventImpl[S, Seq[Any]] {

    peer.changed.--->(changed)(tx0)

    def value(implicit tx: S#Tx): Seq[Any] =
      peer.value.args.toIndexedSeq

    def dispose()(implicit tx: S#Tx): Unit =
      peer.changed.-/->(changed)

    def changed: IChangeEvent[S, Seq[Any]] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): Seq[Any] = {
      val m = pull.expr(peer)
      m.args.toIndexedSeq
    }
  }

  final case class Args(m: Ex[OscMessage]) extends Ex[Seq[Any]] {
    type Repr[S <: Sys[S]] = IExpr[S, Seq[Any]]

    override def productPrefix: String = s"OscMessage$$Args" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ArgsExpanded(m.expand[S], tx)
    }
  }

  // In theory we "should" mixin Caching because we have a side-effect in `pullUpdate`.
  // However, we cannot really do anything useful with the `Var` assignments if we do not
  // use the match trigger and eventually some other action / side-effect.
  private final class SelectExpanded[S <: Sys[S]](m: IExpr[S, OscMessage], name: IExpr[S, String],
                                                  args: Vec[CaseDef.Expanded[S, _]],
                                                  tx0: S#Tx)
                                                 (implicit protected val targets: ITargets[S])
    extends IAction[S] with ITrigger[S] with IGenerator[S, Unit] /* with Caching */ {

    private[this] val numArgs = args.size

    def executeAction()(implicit tx: S#Tx): Unit =
      fire(())

    def addSource(tr: ITrigger[S])(implicit tx: S#Tx): Unit = {
      // ok, this is a bit involved:
      // we must either mixin the trait `Caching` or
      // create an observer to not be eliminated from event
      // reaction execution. If we don't do that, we'd only
      // see activation when our trigger output is otherwise
      // observed (e.g. goes into a `PrintLn`).
      // What we do here is, first, wire the two events together,
      // so that any instancing checking our trigger will observe it
      // within the same event loop as the input trigger, and second,
      // have the observation side effect (`activate`).
      tr.changed ---> changed
    }

    def dispose()(implicit tx: S#Tx): Unit = ()

    private[lucre] def pullUpdate(pull: IPull[S])(implicit tx: S#Tx) : Option[Unit] = {
      if (pull.isOrigin(this)) Trig.Some
      else {
        val p: Parents[S] = pull.parents(this)
        if (p.exists(pull(_).isDefined)) {
          val mv = m    .value
          val av = mv   .args
          val nv = name .value
          val matches = (mv.name == nv) && av.size == numArgs && (numArgs == 0 || {
            var i = 0
            var ok = true
            while (ok && i < numArgs) {
              val a = args(i)
              ok &= a.select(av(i))
              i += 1
            }
            if (ok) {
              i = 0
              while (i < numArgs) {
                val a = args(i)
                a.commit()
                i += 1
              }
            }
            ok
          })
          if (matches) Trig.Some else None

        } else None
      }
    }

    def changed: IEvent[S, Unit] = this
  }

  final case class Select(m: Ex[OscMessage], name: Ex[String], args: CaseDef[_]*) extends Act with Trig {
    type Repr[S <: Sys[S]] = IAction[S] with ITrigger[S]

    override def productPrefix: String = s"OscMessage$$Select" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      val argsEx = args.iterator.map(_.expand[S]).toIndexedSeq
      import ctx.targets
      new SelectExpanded(m.expand[S], name.expand[S], argsEx, tx)
    }
  }

  implicit class Ops(private val m: Ex[OscMessage]) extends AnyVal {
    def name: Ex[String]    = Name(m)
    def args: Ex[Seq[Any]] = Args(m)

    def select(name: Ex[String], args: CaseDef[_]*): Select = Select(m, name, args: _*)
  }

  private final class Expanded[S <: Sys[S]](name: IExpr[S, String], args: IExpr[S, Seq[Any]], tx0: S#Tx)
                                           (implicit protected val targets: ITargets[S])
    extends IExpr[S, OscMessage] with IChangeEventImpl[S, OscMessage] {

    name.changed.--->(changed)(tx0)
    args.changed.--->(changed)(tx0)

    def value(implicit tx: S#Tx): OscMessage = {
      val nameV = name.value
      val argsV = args.value
      new OscMessage(nameV, argsV: _*)
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      name.changed.-/->(changed)
      args.changed.-/->(changed)
    }

    def changed: IChangeEvent[S, OscMessage] = this

    private[lucre] def pullChange(pull: IPull[S])(implicit tx: S#Tx, phase: IPull.Phase): OscMessage = {
      val nameV = pull.expr(name)
      val argsV = pull.expr(args)
      new OscMessage(nameV, argsV: _*)
    }
  }

  private final case class Impl(name: Ex[String], args: Ex[Any]*) extends Ex[OscMessage] {
    type Repr[S <: Sys[S]] = IExpr[S, OscMessage]

    override def productPrefix: String = "OscMessage" // serialization

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      val nameEx = name.expand[S]
      val argsEx = ExSeq(args: _*).expand[S]
      new Expanded[S](nameEx, argsEx, tx)
    }
  }

  // ! important ! override `apply` and make private, so that use site will
  // not accidentally create values, but lift string constants to `Ex[String]`.
  // The `[OscMessage]` just makes the Scala compiler shut up about unused method.
  private[OscMessage] def apply(name: String, args: Any*): OscMessage = new OscMessage(name, args: _*)
}
/** A simplified interface to OSC packets */
final case class OscMessage(name: String, args: Any*) extends OscPacket

//trait OscBundle extends OscPacket {
//  def time: Long
//
//  def packets: Seq[OscPacket]
//}