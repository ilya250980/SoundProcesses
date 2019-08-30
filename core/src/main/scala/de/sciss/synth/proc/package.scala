/*
 *  package.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys

import scala.annotation.elidable
import scala.annotation.elidable.CONFIG

package object proc {
  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'proc' - ", Locale.US)
  var showLog           = false
  var showAuralLog      = false
  var showTransportLog  = false

  type Workspace[S <: Sys[S]] = stm.Workspace[S]

  @elidable(CONFIG) private[sciss] def logAural(what: => String): Unit =
    if (showAuralLog) Console.out.println(s"${logHeader.format(new Date())}aural $what")

  @elidable(CONFIG) private[sciss] def logTransport(what: => String): Unit =
    if (showTransportLog) Console.out.println(s"${logHeader.format(new Date())}transport $what")

  @elidable(CONFIG) private[sciss] def log(what: => String): Unit =
    if (showLog) Console.out.println(logHeader.format(new Date()) + what)

  /** Exception are sometimes swallowed without printing in a transaction. This ensures a print. */
  private[proc] def ???! : Nothing = {
    val err = new NotImplementedError
    err.printStackTrace()
    throw err
  }
}