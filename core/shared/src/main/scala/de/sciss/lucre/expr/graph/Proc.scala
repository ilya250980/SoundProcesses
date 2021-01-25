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

import de.sciss.lucre.Adjunct.HasDefault
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.graph.impl.{AbstractCtxCellView, ExpandedObjMakeImpl, ObjCellViewVarImpl, ObjImplBase}
import de.sciss.lucre.expr.{CellView, Context, IAction}
import de.sciss.lucre.{Adjunct, IExpr, ITargets, Source, StringObj, Sys, Txn, Obj => LObj}
import de.sciss.serial.{DataInput, TFormat}
import de.sciss.{asyncfile, proc}

object Proc extends ProductReader[Ex[Proc]] {
  private lazy val _init: Unit =
    Adjunct.addFactory(Bridge)

  def init(): Unit = _init

  def apply(): Ex[Proc] with Obj.Make = Apply()

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Proc] = {
    require (arity == 0 && adj == 0)
    Proc()
  }

//  private[lucre] def wrapH[T <: Txn[T]](peer: Source[T, proc.Proc[T]], system: Sys): Proc =
//    new Impl[T](peer, system)

  private[lucre] def wrap[T <: Txn[T]](peer: proc.Proc[T])(implicit tx: T): Proc =
    new Impl[T](tx.newHandle(peer), tx.system)

  private[lucre] final class Impl[T <: Txn[T]](in: Source[T, proc.Proc[T]], system: Sys)
    extends ObjImplBase[T, proc.Proc](in, system) with Proc {

    override type Peer[~ <: Txn[~]] = proc.Proc[~]
  }


  private final class CellViewImpl[T <: Txn[T]](h: Source[T, LObj[T]], key: String)
    extends ObjCellViewVarImpl[T, proc.Proc, Proc](h, key) {

    implicit def format: TFormat[T, Option[proc.Proc[T]]] =
      TFormat.option

    protected def lower(peer: proc.Proc[T])(implicit tx: T): Proc =
      wrap(peer)
  }

  implicit object Bridge extends Obj.Bridge[Proc] with HasDefault[Proc] with Adjunct.Factory {
    final val id = 2006

    type Repr[T <: Txn[T]] = proc.Proc[T]

    def defaultValue: Proc = Empty

    override def readIdentifiedAdjunct(in: DataInput): Adjunct = this

    def cellView[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): CellView.Var[T, Option[Proc]] =
      new CellViewImpl(tx.newHandle(obj), key)

    def contextCellView[T <: Txn[T]](key: String)(implicit tx: T, context: Context[T]): CellView[T, Option[Proc]] =
      new AbstractCtxCellView[T, Proc](context.attr, key) {
        protected def tryParseValue(value: Any)(implicit tx: T): Option[Proc] = value match {
          case gr: Proc => Some(gr)
          case _            => None
        }

        protected def tryParseObj(obj: LObj[T])(implicit tx: T): Option[Proc] = obj match {
          case peer: proc.Proc[T] => Some(wrap(peer))
          case _                      => None
        }
      }

    def cellValue[T <: Txn[T]](obj: LObj[T], key: String)(implicit tx: T): Option[Proc] =
      obj.attr.$[proc.Proc](key).map(wrap(_))

    def tryParseObj[T <: Txn[T]](obj: LObj[T])(implicit tx: T): Option[Proc] = obj match {
      case a: proc.Proc[T]  => Some(wrap(a))
      case _                => None
    }
  }

  private[lucre] case object Empty extends Proc {
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