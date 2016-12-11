/*
 *  WorkspaceHandle.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.stm.{NoSys, Sys, TxnLike, Disposable}

object WorkspaceHandle {
  object Implicits {
    implicit def dummy[S <: Sys[S]]: WorkspaceHandle[S] = dummyVal.asInstanceOf[DummyImpl[S]]

    private val dummyVal = new DummyImpl[NoSys]

    private class DummyImpl[S <: Sys[S]] extends WorkspaceHandle[S] {
      def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit = ()
      def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit = ()

      def root(implicit tx: S#Tx): Folder[S] =
        throw new UnsupportedOperationException("No root folder on a dummy workspace handle")
    }
  }
}
trait WorkspaceHandle[S <: Sys[S]] {
  /** Adds a dependent which is disposed just before the workspace is disposed.
    *
    * @param dep  the dependent. This must be an _ephemeral_ object.
    */
  def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike /* S#Tx */): Unit
  def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike /* S#Tx */): Unit

  def root(implicit tx: S#Tx): Folder[S]
}
