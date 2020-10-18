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

import de.sciss.lucre.{Cursor, Disposable, Obj, Txn, Workspace => LWorkspace}
import de.sciss.synth.proc.impl.{GenContextImpl => Impl}

object GenContext {
  def apply[T <: Txn[T]]()(implicit tx: T, cursor: Cursor[T], workspace: LWorkspace[T]): GenContext[T] =
    Impl[T]()
}
/** Context for rendering generated objects. */
trait GenContext[T <: Txn[T]] extends Disposable[T] {
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
  def acquire[A <: Disposable[T]](obj: Obj[T])(init: => A)(implicit tx: T): A

  /** Releases a resource associated with an object.
    * This decreases the use count of the resource, and
    * calls `dispose` on it if the count goes back to zero.
    *
    * @param obj    the object used as a look-up key
    */
  def release(obj: Obj[T])(implicit tx: T): Unit

  /** Attempts to find a resource associated with an object.
    *
    * @param obj    the object used as a look-up key
    * @tparam A     the type of resource
    * @return the resource, if it was found in the cache, or `None`
    */
  def get[A](obj: Obj[T])(implicit tx: T): Option[A]

  implicit def cursor: Cursor[T]

  implicit def workspace: LWorkspace[T]
}