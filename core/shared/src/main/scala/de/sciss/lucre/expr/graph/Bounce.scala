/*
 *  Bounce.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2021 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.lucre.expr.graph

import de.sciss.audiofile.AudioFileSpec
import de.sciss.lucre.expr.Context
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.synth.AnyTxn
import de.sciss.lucre.{Txn, synth}
import de.sciss.proc
import de.sciss.proc.Universe
import de.sciss.span.Span
import de.sciss.synth.{Client, Server}

import java.net.URI

object Bounce extends BouncePlatform with ProductReader[Bounce] {
  // XXX TODO ugly
  private[sciss] var applyAudioPreferences: (Server.ConfigBuilder, Client.ConfigBuilder) => Unit = (_, _) => ()

  def apply(obj: Ex[Seq[Obj]], out: Ex[URI], spec: Ex[AudioFileSpec], span: Ex[Span]): Bounce =
    Impl(obj, out, spec, span)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Bounce = {
    require (arity == 4 && adj == 0)
    val _obj  = in.readEx[Seq[Obj]]()
    val _out  = in.readEx[URI]()
    val _spec = in.readEx[AudioFileSpec]()
    val _span = in.readEx[Span]()
    Bounce(_obj, _out, _spec, _span)
  }

  private final case class Impl(obj: Ex[Seq[Obj]], out: Ex[URI], spec: Ex[AudioFileSpec], span: Ex[Span])
    extends Bounce {

    override def productPrefix: String = "Bounce" // serialization

    type Repr[T <: Txn[T]] = proc.Runner[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      tx match {
        case stx: synth.Txn[_] =>
          // ugly...
          val tup = (ctx, stx).asInstanceOf[(Context[AnyTxn], AnyTxn)]
          mkControlImpl(tup).asInstanceOf[Repr[T]]

        case _ => throw new Exception("Need a SoundProcesses system")
      }

    private def mkControlImpl[T <: synth.Txn[T]](tup: (Context[T], T)): Repr[T] = {
      implicit val ctx: Context[T]  = tup._1
      implicit val tx : T           = tup._2
      import ctx.{cursor, workspace}
      implicit val h  : Universe[T] = Universe()
      new PeerImpl[T](obj.expand[T], out.expand[T], spec.expand[T], span.expand[T])
    }
  }
}
trait Bounce extends Runner {
  def obj : Ex[Seq[Obj]]
  def out : Ex[URI]
  def spec: Ex[AudioFileSpec]
  def span: Ex[Span]
}
