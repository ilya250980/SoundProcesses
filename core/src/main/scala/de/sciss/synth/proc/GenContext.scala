/*
 *  GenContext.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj, Sys}
import de.sciss.synth.proc.impl.{GenContextImpl => Impl}

object GenContext {
  def apply[S <: Sys[S]]()(implicit tx: S#Tx, cursor: stm.Cursor[S], workspace: Workspace[S]): GenContext[S] = Impl[S]
}
/** Context for rendering generated objects. */
trait GenContext[S <: Sys[S]] extends Disposable[S#Tx] {
  /** Acquires a resource associated with an object.
    * The resource is stored under the key `obj.id`,
    * and an internal use count is maintained, calling
    * `dispose` on the resource if the count goes
    * back to zero.
    *
    * @param obj    the object used as a look-up key
    * @param init   the function that produces the resource if it was not yet in the cache
    * @tparam A     the type of resource which must be a `Disposable`
    * @return the resource, either already found in the cache or newly produced
    */
  def acquire[A <: Disposable[S#Tx]](obj: Obj[S])(init: => A)(implicit tx: S#Tx): A

  /** Releases a resource associated with an object.
    * This decreases the use count of the resource, and
    * calls `dispose` on it if the count goes back to zero.
    *
    * @param obj    the object used as a look-up key
    */
  def release(obj: Obj[S])(implicit tx: S#Tx): Unit

  /** Attempts to find a resource associated with an object.
    *
    * @param obj    the object used as a look-up key
    * @tparam A     the type of resource
    * @return the resource, if it was found in the cache, or `None`
    */
  def get[A](obj: Obj[S])(implicit tx: S#Tx): Option[A]

  implicit def cursor: stm.Cursor[S]

  implicit def workspace: Workspace[S]
}