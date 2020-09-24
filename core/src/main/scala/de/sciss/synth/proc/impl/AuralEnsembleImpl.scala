///*
// *  AuralEnsembleImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc.impl
//
//import de.sciss.equal.Implicits._
//import de.sciss.model.Change
//import de.sciss.synth.proc.{AuralContext, AuralObj, Runner, TimeRef, Transport, logTransport}
//
//object AuralEnsembleImpl {
//  def apply[T <: Txn[T]](obj: Ensemble[T], attr: Runner.Attr[T])
//                        (implicit tx: T, context: AuralContext[T]): AuralObj.Ensemble[T] = {
//    val transport = Transport[T](context)
//    val ensemble  = obj
//    ensemble.folder.iterator.foreach(transport.addObject) // XXX TODO --- should we pass `attr`?
//    new Impl(tx.newHandle(obj), transport).init(ensemble)
//  }
//
//  private final class Impl[T <: Txn[T]](objH: stm.Source[T, Ensemble[T]],
//                                        protected val transport: Transport[T])
//    extends AuralFolderLikeImpl[T, /*Ensemble[T],*/ AuralObj.Ensemble[T]]
//    with AuralObj.Ensemble[T] { impl =>
//
//    def tpe: Obj.Type = Ensemble
//
//    def folder(implicit tx: T): Folder[T] = ensemble.folder
//
//    override def obj(implicit tx: T): Ensemble[T] = objH()
//
//    def mkObserver(ens: Ensemble[T])(implicit tx: T): Disposable[T] =
//      ens.changed.react { implicit tx => upd =>
//        val ens = upd.ensemble
//        upd.changes.foreach {
//          case Ensemble.Folder (fUpd) => processFolderUpdate(fUpd)
//          // case Ensemble.Offset (Change(_, newOffset )) =>
//          case Ensemble.Playing(Change(_, newPlaying)) =>
//            logTransport(s"AuralEnsemble - new playing.value = $newPlaying")
//            if (newPlaying) {
//              if (state === Runner.Running) startTransport(ens.offset.value)
//            } else {
//              transport.stop()
//            }
//          case _ =>
//        }
//      }
//
//    private def ensemble(implicit tx: T): Ensemble[T] = objH()
//
//    protected def performPlay(timeRef: TimeRef)(implicit tx: T): Unit = {
//      val ens = ensemble
//      val p   = ens.playing.value
//      logTransport(s"AuralEnsemble.play() - playing.value = $p")
//      if (p) startTransport(ens.offset.value)
//    }
//  }
//}
