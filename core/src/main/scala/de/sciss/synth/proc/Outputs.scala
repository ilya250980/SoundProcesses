/*
 *  Outputs.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.Sys

trait Outputs[S <: Sys[S]] {
  def get(key: String)(implicit tx: S#Tx): Option[Output[S]]

  def keys(implicit tx: S#Tx): Set[String]

  def iterator(implicit tx: S#Tx): Iterator[Output[S]]

  /** Adds a new scan by the given key. If a span by that name already exists, the old scan is returned. */
  def add   (key: String)(implicit tx: S#Tx): Output[S]

  def remove(key: String)(implicit tx: S#Tx): Boolean
}
