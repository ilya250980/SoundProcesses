/*
 *  SynthDefImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2013 Hanns Holger Rutz. All rights reserved.
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
package impl

import de.sciss.synth.{SynthDef => SSynthDef}

private[proc] final case class SynthDefImpl( server: Server, peer: SSynthDef ) extends ResourceImpl with SynthDef {
//   val isOnline = State( this, "isOnline", init = false )

   override def toString = "SynthDef(" + peer.name + ")"

   def name : String = peer.name

   /**
    *    Actually checks if the def is already online.
    *    Only if that is not the case, the receive message
    *    will be queued.
    */
   def recv()( implicit tx: Txn ) {
      tx.addMessage( this, peer.recvMsg, audible = false )
   }

//   def play( target: Node, args: Seq[ ControlSetMap ] = Nil,
//             addAction: AddAction = addToHead, buffers: Seq[ Buffer ] = Nil )( implicit tx: Txn ) : Synth = {
////      recv()  // make sure it is online
//      val rs = Synth( this )
//      rs.play( target, args, addAction, buffers )
//      rs
//   }

   def dispose()( implicit tx: Txn ) {
      require( isOnline )
      tx.addMessage( this, peer.freeMsg, audible = false )
      disposed()
   }
}