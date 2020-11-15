/*
 *  AsyncProcBuilder.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.Txn
import de.sciss.proc.Proc

/** An object used in the preparatory phase of playing a process. It allows
  * the addition of asynchronous processors.
  */
final class AsyncProcBuilder[T <: Txn[T]](val obj: Proc[T]) {
  var resources: List[AsyncResource[T]] = Nil
}