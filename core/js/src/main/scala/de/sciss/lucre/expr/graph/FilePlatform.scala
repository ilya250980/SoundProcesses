/*
 *  FilePlatform.scala
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
import de.sciss.lucre.{Artifact => _Artefact}

import java.net.{URI => _URI}

trait FilePlatform {
  protected def tmpDir: _URI =
    _Artefact.Value.empty
}
