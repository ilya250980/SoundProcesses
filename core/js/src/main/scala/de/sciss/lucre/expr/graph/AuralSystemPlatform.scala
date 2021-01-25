/*
 *  AuralSystemPlatform.scala
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

import de.sciss.lucre.synth.{RT, Server}
import de.sciss.proc
import de.sciss.synth.Client

trait AuralSystemPlatform {
  protected def boot(as: proc.AuralSystem, sCfg: Server.Config, cCfg: Client.Config)(implicit tx: RT): Unit =
    as.connect(sCfg, cCfg)
}
