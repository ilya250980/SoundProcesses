/*
 *  SynthDefPlatform.scala
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

package de.sciss.lucre.synth
package impl

import de.sciss.osc
import de.sciss.synth.{message, SynthDef => SSynthDef}

trait SynthDefPlatform {
  protected def loadDef(peer: SSynthDef): osc.Message with message.Send =
    throw new UnsupportedOperationException("SynthDef is too large to send")
}
