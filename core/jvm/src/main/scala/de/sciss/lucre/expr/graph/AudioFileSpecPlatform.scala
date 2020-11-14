package de.sciss.lucre.expr.graph

import java.io.File
import java.net.URI

import de.sciss.audiofile.{AudioFile, AudioFileSpec => _AudioFileSpec}
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, ITargets, Txn}

import scala.util.Try

trait AudioFileSpecPlatform {
  def read(in: Ex[URI]): Ex[Option[_AudioFileSpec]] = Read(in)

  private final class ReadExpanded[T <: Txn[T]](in: IExpr[T, URI], tx0: T)
                                               (implicit targets: ITargets[T])
    extends MappedIExpr[T, URI, Option[_AudioFileSpec]](in, tx0) {

    protected def mapValue(inValue: URI)(implicit tx: T): Option[_AudioFileSpec] =
      Try {
        val f = new File(inValue)
        AudioFile.readSpec(f)
      } .toOption
  }

  final case class Read(in: Ex[URI]) extends Ex[Option[_AudioFileSpec]] {
    override def productPrefix: String = s"AudioFileSpec$$Read" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[_AudioFileSpec]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ReadExpanded(in.expand[T], tx)
    }
  }
}