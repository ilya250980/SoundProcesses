///*
// *  Ensemble.scala
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
//package de.sciss.proc.impl
//
//import de.sciss.lucre.event.Targets
//import de.sciss.lucre.expr.{BooleanObj, LongObj}
//import de.sciss.lucre.stm.impl.ObjFormat
//import de.sciss.lucre.stm.{Copy, Elem, Folder, NoSys, Obj, Sys}
//import de.sciss.lucre.{event => evt}
//import de.sciss.serial.{DataInput, DataOutput, TFormat}
//import de.sciss.proc.Ensemble
//
//object EnsembleImpl {
//  def apply[T <: Txn[T]](folder: Folder /* Elem.Obj */[T], offset: LongObj[T], playing: BooleanObj[T])
//                        (implicit tx: T): Ensemble[T] = {
//    val targets = evt.Targets[T]
//    new Impl(targets, folder, offset, playing).connect()
//  }
//
//  def format[T <: Txn[T]]: TFormat[T, Ensemble[T]] = anySer.asInstanceOf[Ser[T]]
//
//  private val anySer = new Ser[NoSys]
//
//  private final class Ser[T <: Txn[T]] extends ObjFormat[T, Ensemble[T]] {
//    def tpe: Obj.Type = Ensemble
//  }
//
//  def readIdentifiedObj[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): Ensemble[T] with evt.Node[T] = {
//    val targets = Targets   .read(in, access)
//    val folder  = Folder    .read(in, access)
//    // val folder  = Obj.readT[T, FolderElem](in, access)
//    val offset  = LongObj   .read(in, access)
//    val playing = BooleanObj.read(in, access)
//    new Impl(targets, folder, offset, playing)
//  }
//
////  def mkCopy()(implicit tx: T): Ensemble.Elem[T] = {
////    // val folderOrig  = peer.folder
////    // val folderCopy: FolderElem.Obj[T] = Obj.copyT[T, FolderElem](folderOrig, folderOrig.elem)  // P.I.T.A.
////    // val folderCopy = Folder[T]
////    // folderOrig.iterator.foreach(folderCopy.addLast)
////    val folderCopy  = peer.folder   // XXX TODO
////    val offsetCopy  = peer.offset   // XXX TODO
////    val playingCopy = peer.playing  // XXX TODO
////    val copy = Ensemble(folderCopy, offsetCopy, playingCopy)
////    Ensemble.Elem(copy)
////  }
//
//  // ---- impl ----
//
//  private final class Impl[T <: Txn[T]](val targets: evt.Targets[T], _folder: Folder[T],
//                                        _offset: LongObj[T], _playing: BooleanObj[T])
//    extends Ensemble[T]
//    with evt.impl.SingleNode[T, Ensemble.Update[T]] { self =>
//
//    def tpe: Obj.Type = Ensemble
//
//    override def toString: String = s"Ensemble$id"
//
//    def folder (implicit tx: T): Folder    [T] = _folder
//    def offset (implicit tx: T): LongObj   [T] = _offset
//    def playing(implicit tx: T): BooleanObj[T] = _playing
//
//    def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
//      new Impl(Targets[Out], context(_folder), context(_offset), context(_playing)).connect()
//
//    protected def writeData(out: DataOutput): Unit = {
//      _folder .write(out)
//      _offset .write(out)
//      _playing.write(out)
//    }
//
//    protected def disposeData()(implicit tx: T): Unit = disconnect()
//
//    // ---- event ----
//
//    def connect()(implicit tx: T): this.type = {
//      _folder .changed ---> changed
//      _offset .changed ---> changed
//      _playing.changed ---> changed
//      this
//    }
//
//    private def disconnect()(implicit tx: T): Unit = {
//      _folder .changed -/-> changed
//      _offset .changed -/-> changed
//      _playing.changed -/-> changed
//    }
//
//    object changed extends Changed {
//      def pullUpdate(pull: evt.Pull[T])(implicit tx: T): Option[Ensemble.Update[T]] = {
//        val folderEvt = _folder /* .elem.peer */ .changed
//        val l1 = if (pull.contains(folderEvt))
//          pull(folderEvt).fold(List.empty[Ensemble.Change[T]])(Ensemble.Folder(_) :: Nil)
//        else Nil
//
//        val offsetEvt = _offset.changed
//        val l2 = if (pull.contains(offsetEvt))
//          pull(offsetEvt).fold(l1)(Ensemble.Offset[T](_) :: l1)
//        else l1
//
//        val playingEvt = _playing.changed
//        val l3 = if (pull.contains(playingEvt))
//          pull(playingEvt).fold(l2)(Ensemble.Playing[T](_) :: l2)
//        else l2
//
//        if (l3.isEmpty) None else Some(Ensemble.Update(self, l3))
//      }
//    }
//  }
//}
