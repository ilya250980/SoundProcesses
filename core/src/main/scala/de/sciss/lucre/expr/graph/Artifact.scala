/*
 *  Artifact.scala
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
import de.sciss.lucre.artifact.{ArtifactLocation, Artifact => _Artifact}
import de.sciss.lucre.aux.Aux
import de.sciss.lucre.expr.impl.CellViewImpl
import de.sciss.lucre.expr.impl.CellViewImpl.AttrMapExprObs
import de.sciss.lucre.expr.{CellView, Context, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.serial.{DataInput, Serializer}

import scala.util.{Failure, Success, Try}

object Artifact {
  private lazy val _init: Unit = Aux.addFactory(Bridge)

  def init(): Unit = _init

  private final object Bridge extends Obj.Bridge[File] with Aux.Factory {
    final val id = 2000

    type Repr[S <: Sys[S]] = _Artifact[S]

    def readIdentifiedAux(in: DataInput): Aux = this

    def mkObj[S <: Sys[S]](f: File)(implicit tx: S#Tx): _Artifact[S] = {
      val loc = defaultLocation(f)
      makeArtifact(loc, f)
    }

    implicit def reprSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, _Artifact[S]] =
      _Artifact.serializer

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[File]] =
      new CellViewImpl(tx.newHandle(obj.attr), key = key)
  }

  private def tryRelativize[S <: Sys[S]](loc: ArtifactLocation[S], f: File)(implicit tx: S#Tx): Try[_Artifact.Child] =
    Try(_Artifact.relativize(loc.directory, f))

  private def defaultLocation[S <: Sys[S]](f: File)(implicit tx: S#Tx): ArtifactLocation[S] =
    ArtifactLocation.newVar(f.absolute.parent)

  private def makeArtifact[S <: Sys[S]](loc: ArtifactLocation[S], f: File)(implicit tx: S#Tx): _Artifact[S] = {
    val art = tryRelativize(loc, f).toOption.fold[_Artifact[S]]({ // Try#fold not available in Scala 2.11
      _Artifact(defaultLocation(f), f)
    }) { child =>
      _Artifact(loc, child)
    }
    art
  }

  private final class CellViewImpl[S <: Sys[S]](attrH: stm.Source[S#Tx, stm.Obj.AttrMap[S]], key: String)
    extends CellView.Var[S, Option[File]] with CellViewImpl.Basic[S#Tx, Option[File]] {

    private def attr(implicit tx: S#Tx): stm.Obj.AttrMap[S] = attrH()

    type Repr = Option[_Artifact[S]]

    def serializer: Serializer[S#Tx, S#Acc, Repr] = Serializer.option

    def repr(implicit tx: S#Tx): Repr = 
      attr.$[_Artifact](key)

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit =
      value match {
        case Some(a)  => attr.put(key, a)
        case None     => attr.remove(key)
      }

    def lift(v: Option[File])(implicit tx: S#Tx): Repr =
      v match {
        case Some(f) if f.path.nonEmpty =>
          val loc = repr.fold[ArtifactLocation[S]](defaultLocation(f))(_.location)
          val art = makeArtifact(loc, f)
          Some(art)

        case _ => None
      }
    
    def apply()(implicit tx: S#Tx): Option[File] = repr.map(_.value)

    def update(v: Option[File])(implicit tx: S#Tx): Unit = {
      def fallback(): Unit = repr_=(lift(v))

      v match {
        case Some(f) if f.path.nonEmpty =>
          repr match {
            case Some(am: _Artifact.Modifiable[S]) =>
              tryRelativize(am.location, f) match {
                case Success(child) => am.child = child
                case Failure(_) => fallback()
              }
            case _ => fallback()
          }
        case _ => fallback()
      }
    }

    def react(fun: S#Tx => Option[File] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      new AttrMapExprObs[S, File](map = attr, key = key, fun = fun, tx0 = tx)(_Artifact)
  }
}
final case class Artifact(key: String, default: Ex[File] = file(""))
  extends Attr.WithDefault[File] {

  type Repr[S <: Sys[S]] = IExpr[S, File]

  protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
    val defaultEx: Repr[S] = default.expand[S]
    Attr.resolveNested(key).fold(defaultEx) { attrView =>
      import ctx.targets
      new Attr.WithDefault.Expanded[S, File](attrView, defaultEx, tx)
    }
  }

  def update(in: Ex[File]): Control = Attr.Update (in, key)
  def set   (in: Ex[File]): Act     = Attr.Set    (in, key)

  implicit def bridge: Obj.Bridge[File] = Artifact.Bridge

  def aux: List[Aux] = Nil
}
