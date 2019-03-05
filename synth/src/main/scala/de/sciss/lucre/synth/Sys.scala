/*
 *  Sys.scala
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

package de.sciss.lucre.synth

import de.sciss.lucre.{stm, synth}

object Sys {
  trait Txn[S <: Sys[S]] extends stm.Txn[S] with synth.Txn
}

trait Sys[S <: Sys[S]] extends stm.Sys[S] {
  type Tx <: Sys.Txn[S]
  type I <: InMemoryLike[I]
}

trait NoSys extends Sys[NoSys]