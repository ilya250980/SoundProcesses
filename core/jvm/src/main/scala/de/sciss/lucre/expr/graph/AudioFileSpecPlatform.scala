package de.sciss.lucre.expr.graph

import de.sciss.file.File
import de.sciss.lucre.expr.Context
import de.sciss.lucre.{IExpr, ITargets, Txn}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.audiofile.AudioFile

import scala.util.Try
import de.sciss.audiofile.{AudioFileSpec => _AudioFileSpec}

trait AudioFileSpecPlatform {
  def read(in: Ex[File]): Ex[Option[_AudioFileSpec]] = Read(in)

  private final class ReadExpanded[T <: Txn[T]](in: IExpr[T, File], tx0: T)
                                               (implicit targets: ITargets[T])
    extends MappedIExpr[T, File, Option[_AudioFileSpec]](in, tx0) {

    protected def mapValue(inValue: File)(implicit tx: T): Option[_AudioFileSpec] =
      Try(
        AudioFile.readSpec(inValue)
      ).toOption
  }

  final case class Read(in: Ex[File]) extends Ex[Option[_AudioFileSpec]] {
    override def productPrefix: String = s"AudioFileSpec$$Read" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Option[_AudioFileSpec]]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new ReadExpanded(in.expand[T], tx)
    }
  }
}