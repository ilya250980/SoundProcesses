///*
// *  AuralView.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Lesser General Public License v2.1+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc
//
//import de.sciss.lucre.stm
//import de.sciss.lucre.stm.{Obj, Sys}
//
///** A trait that provides a shared structure for `AuralObj` and `AuralAttribute`,
//  * the only difference being the `Target` context type needed for issuing a play.
//  */
//trait AuralView[S <: Sys[S], -Target] extends RunnerBase[S, Target] {
//  def tpe: Obj.Type
//
//  /** The view must store a handle to its underlying model. */
//  def objH: stm.Source[S#Tx, Obj[S]]
//}