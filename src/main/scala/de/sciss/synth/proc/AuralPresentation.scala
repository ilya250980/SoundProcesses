/*
 *  AuralPresentation.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import impl.AuralPresentationImpl
import de.sciss.lucre.stm.{Disposable, Cursor, Sys}

object AuralPresentation {
   // ---- implementation forwards ----

   def run[ S <: Sys[ S ]]( transport: Transport[ S, Proc[ S ]], aural: AuralSystem )
                          ( implicit tx: S#Tx, cursor: Cursor[ S ]) : AuralPresentation[ S ] =
      AuralPresentationImpl.run( transport, aural )

   private[proc] trait Running[ S <: Sys[ S ]] {
      def scanInValue( proc: Proc[ S ], time: Long, key: String )( implicit tx: S#Tx ) : Option[ Scan_.Value[ S ]]
   }
}
trait AuralPresentation[ S <: Sys[ S ]] extends Disposable[ S#Tx ]
