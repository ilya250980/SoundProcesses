/*
 *  AudioFileSpecPlatform.scala
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

import java.net.URI

import de.sciss.audiofile.{AudioFileSpec => _AudioFileSpec}
import de.sciss.lucre.expr.graph.impl.MappedIExpr
import de.sciss.lucre.{IExpr, ITargets, Txn}

trait AudioFileSpecPlatform {
  protected final class ReadExpanded[T <: Txn[T]](in: IExpr[T, URI], tx0: T)
                                                 (implicit targets: ITargets[T])
    extends MappedIExpr[T, URI, Option[_AudioFileSpec]](in, tx0) {

    protected def mapValue(inValue: URI)(implicit tx: T): Option[_AudioFileSpec] = {
      Console.err.println("AudioFileSpec.Read: unsupported on .js")
      None
    }
  }
}