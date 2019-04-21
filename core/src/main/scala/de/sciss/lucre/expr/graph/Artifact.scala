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
import de.sciss.lucre.expr.ExOps._
import de.sciss.lucre.expr.impl.CellViewImpl
import de.sciss.lucre.expr.impl.CellViewImpl.AttrMapExprObs
import de.sciss.lucre.expr.{CellView, Control, Ex, IExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.serial.{DataInput, Serializer}

import scala.util.{Failure, Success, Try}

object Artifact {
  private lazy val _init: Unit = Aux.addFactory(Bridge)

  def init(): Unit = _init

  private final object Bridge extends Attr.Bridge[File] with Aux.Factory {

    def readIdentifiedAux(in: DataInput): Aux = this

    def cellView[S <: Sys[S]](obj: stm.Obj[S], key: String)(implicit tx: S#Tx): CellView.Var[S, Option[File]] =
      new CellViewImpl(tx.newHandle(obj.attr), key = key)

    final val id = 2000
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

    private def tryRelativize(loc: ArtifactLocation[S], f: File)(implicit tx: S#Tx): Try[_Artifact.Child] =
      Try(_Artifact.relativize(loc.directory, f))

    def lift(v: Option[File])(implicit tx: S#Tx): Repr =
      v match {
        case Some(f) if f.path.nonEmpty =>
          def defaultLoc(): ArtifactLocation[S] = ArtifactLocation.newVar(f.absolute.parent)

          val loc = repr.fold[ArtifactLocation[S]]({
            defaultLoc()
          })(_.location)
          val art = tryRelativize(loc, f).toOption.fold[_Artifact[S]]({ // Try#fold not available in Scala 2.11
            _Artifact(defaultLoc(), f)
          }) { child =>
            _Artifact(loc, child)
          }
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

  def expand[S <: Sys[S]](implicit ctx: Ex.Context[S], tx: S#Tx): IExpr[S, File] = {
    val defaultEx = default.expand[S]
    Attr.resolveNested(key).fold(defaultEx) { attrView =>
      import ctx.targets
      new Attr.WithDefault.Expanded[S, File](attrView, defaultEx, tx)
    }
  }

  def update(in: Ex[File]): Control = Attr.Update(in, key)

  implicit def bridge: Attr.Bridge[File] = Artifact.Bridge

  def aux: List[Aux] = Nil
}
