/*
 *  Proc.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.file._
import de.sciss.lucre.event.ITargets
import de.sciss.lucre.expr.graph.impl.{ExpandedObjMakeImpl, ObjImplBase}
import de.sciss.lucre.expr.{Context, IAction, IExpr, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc
import de.sciss.synth.proc.{ObjKeys, SynthGraphObj, AudioCue => _AudioCue}

object Proc {
  def apply(): Ex[Proc] with Obj.Make = Apply()

  object Tape {
    def apply(cue: Ex[_AudioCue]): Ex[Proc] with Obj.Make = TapeImpl(cue)

    private final case class TapeImpl(cue: Ex[_AudioCue]) extends Ex[Proc] with Act with Obj.Make {
      override def productPrefix: String = s"Proc$$Tape" // serialization

      type Repr[S <: Sys[S]] = IExpr[S, Proc] with IAction[S]

      def make: Act = this

      protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
        import ctx.targets
        new TapeExpanded(cue.expand[S])
      }
    }
  }

  private final class TapeExpanded[S <: Sys[S]](cue: IExpr[S, _AudioCue])(implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Proc] {

    protected def empty: Proc = Empty

    protected def make()(implicit tx: S#Tx): Proc = {
      val peer    = proc.Proc[S]()
      val a       = peer.attr
      val cueV    = cue.value
      val name    = StringObj     .newVar[S](cueV.artifact.base)
      val cueObj  = _AudioCue.Obj .newVar[S](cueV)
      a.put(ObjKeys   .attrName   , name)
      a.put(proc.Proc .graphAudio , cueObj)
      peer.graph() = SynthGraphObj.tape
      a.put(proc.Proc.attrSource, SynthGraphObj.tapeSource)
      peer.outputs.add(proc.Proc.mainOut)
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private[lucre] def wrap[S <: Sys[S]](peer: stm.Source[S#Tx, proc.Proc[S]], system: S): Proc =
    new Impl[S](peer, system)

  private final class Impl[S <: Sys[S]](in: stm.Source[S#Tx, proc.Proc[S]], system: S)
    extends ObjImplBase[S, proc.Proc](in, system) with Proc {

    override type Peer[~ <: Sys[~]] = proc.Proc[~]
  }

  private[lucre] object Empty extends Proc {
    private[lucre] def peer[S <: Sys[S]](implicit tx: S#Tx): Option[Peer[S]] = None
  }

  private final class ApplyExpanded[S <: Sys[S]](implicit targets: ITargets[S])
    extends ExpandedObjMakeImpl[S, Proc] {

    protected def empty: Proc = Empty

    protected def make()(implicit tx: S#Tx): Proc = {
      val peer = proc.Proc[S]()
      new Impl(tx.newHandle(peer), tx.system)
    }
  }

  private final case class Apply() extends Ex[Proc] with Act with Obj.Make {
    override def productPrefix: String = "Proc" // serialization

    type Repr[S <: Sys[S]] = IExpr[S, Proc] with IAction[S]

    def make: Act = this

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.targets
      new ApplyExpanded[S]
    }
  }
}
trait Proc extends Obj {
  type Peer[~ <: Sys[~]] = proc.Proc[~]
}