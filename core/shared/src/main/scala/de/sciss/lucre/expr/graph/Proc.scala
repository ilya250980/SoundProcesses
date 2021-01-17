/*
 *  Proc.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.{ExpandedObjMakeImpl, ObjImplBase}
import de.sciss.lucre.expr.{Context, IAction}
import de.sciss.lucre.{IExpr, ITargets, Source, StringObj, Sys, Txn}
import de.sciss.{asyncfile, proc}

object Proc extends ProductReader[Ex[Proc]] {
  def apply(): Ex[Proc] with Obj.Make = Apply()

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Proc] = {
    require (arity == 0 && adj == 0)
    Proc()
  }

  private[lucre] def wrap[T <: Txn[T]](peer: Source[T, proc.Proc[T]], system: Sys): Proc =
    new Impl[T](peer, system)

  private[lucre] final class Impl[T <: Txn[T]](in: Source[T, proc.Proc[T]], system: Sys)
    extends ObjImplBase[T, proc.Proc](in, system) with Proc {

    override type Peer[~ <: Txn[~]] = proc.Proc[~]
  }

  private[lucre] object Empty extends Proc {
    private[lucre] def peer[T <: Txn[T]](implicit tx: T): Option[Peer[T]] = None
  }

  private final class ApplyExpanded[T <: Txn[T]](implicit targets: ITargets[T])
    extends ExpandedObjMakeImpl[T, Proc] {

    protected def empty: Proc = Empty

    protected def make()(implicit tx: T): Proc = {
      val peer = proc.Proc[T]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Proc] with Act with Obj.Make {
    override def productPrefix: String = "Proc" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Proc] with IAction[T]

    def make: Act = this

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ApplyExpanded[T]
    }
  }

  object Tape extends ProductReader[Ex[Proc]] {
    def apply(cue: Ex[proc.AudioCue]): Ex[Proc] with Obj.Make = TapeImpl(cue)

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Proc] = {
      require (arity == 1 && adj == 0)
      val _cue = in.readEx[proc.AudioCue]()
      Tape(_cue)
    }

    private final case class TapeImpl(cue: Ex[proc.AudioCue]) extends Ex[Proc] with Act with Obj.Make {
      override def productPrefix: String = s"Proc$$Tape" // serialization

      type Repr[T <: Txn[T]] = IExpr[T, Proc] with IAction[T]

      def make: Act = this

      protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
        import ctx.targets
        new TapeExpanded(cue.expand[T])
      }
    }
  }

  private final class TapeExpanded[T <: Txn[T]](cue: IExpr[T, proc.AudioCue])(implicit targets: ITargets[T])
    extends ExpandedObjMakeImpl[T, Proc] {

    protected def empty: Proc = Proc.Empty

    protected def make()(implicit tx: T): Proc = {
      val peer    = proc.Proc[T]()
      val a       = peer.attr
      val cueV    = cue.value
      import asyncfile.Ops._
      val name    = StringObj         .newVar[T](cueV.artifact.base)
      val cueObj  = proc.AudioCue.Obj .newVar[T](cueV)
      a.put(proc.ObjKeys.attrName   , name)
      a.put(proc.Proc   .graphAudio , cueObj)
      peer.graph() = proc.Proc.GraphObj.tape
      a.put(proc.Proc.attrSource, proc.Proc.GraphObj.tapeSource)
      peer.outputs.add(proc.Proc.mainOut)
      new Proc.Impl(tx.newHandle(peer), tx.system)
    }
  }
}
trait Proc extends Obj {
  type Peer[~ <: Txn[~]] = proc.Proc[~]
}