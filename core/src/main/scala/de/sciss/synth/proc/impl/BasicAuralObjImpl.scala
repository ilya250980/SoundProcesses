/*
 *  BasicAuralObjImpl.scala
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

package de.sciss.synth.proc.impl

import de.sciss.lucre.stm.Sys
import de.sciss.synth.proc.AuralObj

trait BasicAuralObjImpl[S <: Sys[S]] extends AuralObj[S] with BasicViewBaseImpl[S, Unit]