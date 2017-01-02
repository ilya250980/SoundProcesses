/*
 *  Gen.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.{Obj, Sys}

import scala.util.Try

trait Gen[S <: Sys[S]] extends Obj[S] {
  def valueType: Obj.Type
  def value(implicit tx: S#Tx): Option[Try[Obj[S]]] // analogous to `Future`

//  def render()   (implicit tx: S#Tx):        Processor[Obj[S]]
//  def rendering  (implicit tx: S#Tx): Option[Processor[Obj[S]]]
//  def isRendering(implicit tx: S#Tx): Boolean
}
