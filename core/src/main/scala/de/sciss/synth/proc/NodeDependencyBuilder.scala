/*
 *  NodeDependencyBuilder.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2015 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.{Node, DynamicUser, Resource, Sys}
import de.sciss.synth.ControlSet

trait NodeDependencyBuilder[S <: Sys[S]] {
  def obj: Obj[S]

  def node: Node

  /** Adds an `n_set` pair to the builder. */
  def addControl(pair: ControlSet): Unit

  /** Adds a _keyed_ user. This is a user that is
    * associated with an attribute key. Only if
    * the attribute is used, will the user become
    * effective. If this is not desired, the
    * regular `users ::= _` mechanism should be used.
    */
  def addUser    (user    : DynamicUser): Unit

  /** Adds a _keyed_ resource. This is a resource that is
    * associated with an attribute key. Only if
    * the attribute is used, will the resource become
    * effective. If this is not desired, the
    * regular `dependencies ::= _` mechanism should be used.
    */
  def addResource(resource: Resource   ): Unit
}