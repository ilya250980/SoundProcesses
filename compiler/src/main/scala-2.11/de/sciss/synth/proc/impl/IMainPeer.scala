/*
 *  IMainPeer.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import scala.tools.nsc.{ConsoleWriter, Global, NewLinePrintWriter}
import scala.tools.nsc.interpreter.IMain

// this is the trick to get the right class-path -- we steal it from the macro compiler
private[impl] final class IMainPeer(val peer: Global)
  extends IMain(peer.settings.copy(), new NewLinePrintWriter(new ConsoleWriter, autoFlush = true))
