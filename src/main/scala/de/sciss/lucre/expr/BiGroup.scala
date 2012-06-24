/*
 *  BiGroup.scala
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

package de.sciss.lucre.expr

import de.sciss.lucre.{event => evt, DataInput}
import evt.{Event, EventLike}
import de.sciss.collection.txn
import de.sciss.lucre.stm.{TxnSerializer, Sys}
import impl.BiGroupImpl
import collection.immutable.{IndexedSeq => IIdxSeq}

object BiGroup {
   sealed trait Update[ S <: Sys[ S ], Elem, U ] {
      def group: BiGroup[ S, Elem, U ]
   }
   sealed trait Collection[ S <: Sys[ S ], Elem, U ] extends Update[ S, Elem, U ] {
//      def elem: Elem
//      def span: SpanLike
   }
   final case class Added[   S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], span: SpanLike, elem: Elem ) extends Collection[ S, Elem, U ]
   final case class Removed[ S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], span: SpanLike, elem: Elem ) extends Collection[ S, Elem, U ]
   final case class Moved[   S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], changes: IIdxSeq[ (evt.Change[ SpanLike ], Elem) ]) extends Collection[ S, Elem, U ]
   final case class Element[ S <: Sys[ S ], Elem, U ]( group: BiGroup[ S, Elem, U ], changes: IIdxSeq[ (Elem, U) ]) extends Update[ S, Elem, U ]

   type TimedElem[ S <: Sys[ S ], Elem ] = (Expr[ S, SpanLike ], Elem)
   type Leaf[      S <: Sys[ S ], Elem ] = (SpanLike, IIdxSeq[ TimedElem[ S, Elem ]])

   def newVar[ S <: Sys[ S ], A ]( implicit tx: S#Tx, elemType: BiType[ A ]) : Var[ S, Expr[ S, A ], evt.Change[ A ]] =
      BiGroupImpl.newGenericVar[ S, Expr[ S, A ], evt.Change[ A ]]( _.changed )( tx, elemType.serializer[ S ], elemType.spanLikeType )

   def newGenericVar[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
      ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
        spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = BiGroupImpl.newGenericVar( eventView )

   def readGenericVar[ S <: Sys[ S ], Elem, U ]( in: DataInput, access: S#Acc, eventView: Elem => EventLike[ S, U, Elem ])
         ( implicit tx: S#Tx, elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
           spanType: Type[ SpanLike ]) : Var[ S, Elem, U ] = BiGroupImpl.readGenericVar( in, access, eventView )

   trait Var[ S <: Sys[ S ], Elem, U ] extends BiGroup[ S, Elem, U ] {
      def add(    span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Unit
      def remove( span: Expr[ S, SpanLike ], elem: Elem )( implicit tx: S#Tx ) : Boolean
      def clear()( implicit tx: S#Tx ) : Unit
   }

   def serializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                           ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                             spanType: Type[ SpanLike ]) : TxnSerializer[ S#Tx, S#Acc, BiGroup[ S, Elem, U ]] =
      BiGroupImpl.serializer[ S, Elem, U ]( eventView )

   def varSerializer[ S <: Sys[ S ], Elem, U ]( eventView: Elem => EventLike[ S, U, Elem ])
                                              ( implicit elemSerializer: TxnSerializer[ S#Tx, S#Acc, Elem ] with evt.Reader[ S, Elem ],
                                                spanType: Type[ SpanLike ]) : TxnSerializer[ S#Tx, S#Acc, BiGroup.Var[ S, Elem, U ]] =
      BiGroupImpl.varSerializer[ S, Elem, U ]( eventView )
}
trait BiGroup[ S <: Sys[ S ], Elem, U ] extends evt.Node[ S ] {
   import BiGroup.Leaf

//   /**
//    * Generates an iterator over all elements in the group which intersect (whose span contains)
//    * the current time as given by the implicit `chronos` argument.
//    *
//    * This methods makes no guarantees about the ordering of the returned iterator.
//    *
//    * @param chronos a reference to the current time cursor
//    * @return  a (possibly empty) iterator of the intersecting elements
//    */
//   def iterator( implicit tx: S#Tx, chronos: Chronos[ S ]) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]

   /**
    * Queries all elements intersecting a given point in time.
    * That is, returns an iterator of all elements whose span contains the time point
    * `(span start <= time && span.stop > time)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param time the point in time to search at
    * @return  a (possibly empty) iterator of the intersecting elements
    */
   def intersect( time: Long )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]

   /**
    * Queries all elements intersecting a given time span.
    * That is, returns an iterator of all elements whose span contains or partly overlaps the query span.
    * `(span start < query.stop && span.stop > query.start)`
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param span the the span to search within (this may be a half-bounded interval or even `Span.All`)
    * @return  a (possibly empty) iterator of the intersecting elements
    */
   def intersect( span: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]

   /**
    * Performs a range query according to separate intervals for the allowed start and stop positions
    * of the element spans. That is, returns an iterator of all elements whose span satisfies the
    * constraints given for start and stop positions
    * `(start.contains( elem.span.start ) && stop.contains( elem.span.stop ))`
    *
    * Both for the start and stop constraint, half-bounded or unbounded (`Span.All`) intervals can be used.
    * Examples
    *
    *  - to find all elements which start between 10 (inclusive) and 20 (exclusive), use `start = Span( 10, 20 ), stop = Span.All`.
    *  - to find all elements which start before (<) 10 and stop from (>=) 20, use `start = Span.until( 10 ), stop = Span.from( 20 )`.
    *
    * This methods makes no guarantees about the ordering of the returned iterator.
    *
    * @param start   the constraint for the start position of the spans of the elements filtered.
    * @param stop    the constraint for the stop position of the spans of the elements filtered.
    * @return  a (possibly empty) iterator of the intersecting elements
    */
   def rangeSearch( start: SpanLike, stop: SpanLike )( implicit tx: S#Tx ) : txn.Iterator[ S#Tx, Leaf[ S, Elem ]]

   /**
    * Queries the closest event (an element's span starting or stopping) no earlier than the given time
    *
    * @param time the query time
    * @return a time, greater than or equal to the query time, at which the next event occurs, or `None` if
    *         there are no events after the query time
    */
   def nearestEventAfter( time: Long )( implicit tx: S#Tx ) : Option[ Long ]

   /**
    * Queries the closest event (an element's span starting or stopping) no later than the given time
    *
    * @param time the query time
    * @return a time, smaller than or equal to the query time, at which the previous event occurs, or `None` if
    *         there are no events before the query time
    */
   def nearestEventBefore( time: Long )( implicit tx: S#Tx ) : Option[ Long ]

   /**
    * Queries all elements which produce an event (starting or stopping) at a given time.
    *
    * @param time the time instant for which to gather the events
    * @return  a tuple of two iterators. the first iterator (`_1`) contains the events which
    *          start at the query time, the second iterator (_2) contains the event which
    *          stop at the query time
    */
   def eventsAt( time: Long )( implicit tx: S#Tx ) : (txn.Iterator[ S#Tx, Leaf[ S, Elem ]], txn.Iterator[ S#Tx, Leaf[ S, Elem ]])

//   def projection( implicit tx: S#Tx, time: Chronos[ S ]) : Expr[ S, A ]

   def collectionChanged:  Event[ S, BiGroup.Collection[ S, Elem, U ], BiGroup[ S, Elem, U ]]
   def elementChanged:     Event[ S, BiGroup.Element[    S, Elem, U ], BiGroup[ S, Elem, U ]]
   def changed :           Event[ S, BiGroup.Update[     S, Elem, U ], BiGroup[ S, Elem, U ]]

   def debugList()( implicit tx: S#Tx ) : List[ (SpanLike, Elem) ]
}