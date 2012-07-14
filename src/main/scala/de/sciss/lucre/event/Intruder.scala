package de.sciss.lucre.event

import de.sciss.lucre.stm.Sys

object Intruder {
   def --->[ S <: Sys[ S ]]( e: EventLike[ S, _, _ ], sel: Selector[ S ])( implicit tx: S#Tx ) {
      e ---> sel
   }

   def -/->[ S <: Sys[ S ]]( e: EventLike[ S, _, _ ], sel: Selector[ S ])( implicit tx: S#Tx ) {
      e -/-> sel
   }

   def devirtualize[ S <: Sys[ S ]]( sel: VirtualNodeSelector[ S ], reader: Reader[ S, Node[ S ]])( implicit tx: S#Tx ) : NodeSelector[ S, _ ] =
      sel.devirtualize( reader )

   def devirtualizeNode[ S <: Sys[ S ]]( sel: VirtualNodeSelector[ S ], reader: Reader[ S, Node[ S ]])( implicit tx: S#Tx ) : Node[ S ] =
      sel.devirtualize( reader ).node

   def pullUpdate[ S <: Sys[ S ], A ]( sel: NodeSelector[ S, A ], pull: Pull[ S ])( implicit tx: S#Tx ) : Option[ A ] =
      sel.pullUpdate( pull )

   def isSource[ S <: Sys[ S ]]( evt: Event[ S, _, _ ], pull: Pull[ S ]) : Boolean = evt.isSource( pull )
}
