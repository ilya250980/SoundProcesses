/*
 *  ProcRunnerImpl.scala
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

package de.sciss.synth.proc
package impl

import de.sciss.lucre.synth.{Sys => SSys}

object ProcRunnerImpl {
  def apply[S <: SSys[S]](obj: Proc[S])(implicit tx: S#Tx, universe: Runner.Universe[S]): Runner[S] =
    BasicRunnerImpl(obj)
}