/*
 *  ServerBase.scala
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

package de.sciss.lucre.synth.impl

import de.sciss.lucre.synth.Server
import de.sciss.osc

import scala.collection.immutable.ArraySeq

// efficient bundle creation from array
private abstract class ServerBase extends Server {
  protected final def mkBundle(tt: osc.TimeTag, packets: Array[osc.Packet], numPackets: Int): osc.Bundle = {
    val outT = new Array[osc.Packet](numPackets)
    System.arraycopy(packets, 0, outT, 0, numPackets)
    osc.Bundle(tt, new ArraySeq.ofRef(outT): _*)
  }
}
