/*
 *  CursorsImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.confluent.Cursor.Data
import de.sciss.lucre.confluent.{Access, Txn => KTxn}
import de.sciss.lucre.impl.SingleEventNode
import de.sciss.lucre.{Copy, Disposable, DurableLike, Elem, ListObj, Pull, StringObj, Txn, confluent}
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable}
import de.sciss.synth.proc.{Confluent, Cursors, Durable, log}

object CursorsImpl {
  private final val COOKIE = 0x43737273 // "Csrs"

  def apply[T <: KTxn[T], D1 <: Txn[D1]](seminal: Access[T])
                                         (implicit tx: D1): Cursors[T, D1] = {
    val targets = Targets[D1]()
    val cursor  = confluent.Cursor.Data[T, D1](seminal)
    val name    = StringObj.newVar[D1]("branch")
    type CursorAux[~ <: Txn[~]] = Cursors[T, ~]
    val list    = ListObj.Modifiable[D1, CursorAux]
    log(s"Cursors.apply targets = $targets, list = $list")
    new Impl(targets, seminal, cursor, name, list).connect()
  }

  implicit def format[T <: KTxn[T], D1 <: Txn[D1]]:
    TFormat[D1, Cursors[T, D1]] = new Fmt[T, D1]

  private final class Fmt[T <: KTxn[T], D1 <: Txn[D1]]
    extends TFormat[D1, Cursors[T, D1]] {

    def write(v: Cursors[T, D1], out: DataOutput): Unit = v.write(out)

    def readT(in: DataInput)(implicit tx: D1): Cursors[T, D1] = {
      val tpe     = in.readInt()
      if (tpe != Cursors.typeId) sys.error(s"Type mismatch, found $tpe, expected ${Cursors.typeId}")
      readIdentified1[T, D1](in)
    }
  }

  def readIdentifiedObj[T <: Txn[T]](in: DataInput)(implicit tx: T): Elem[T] = {
    if (!tx.system.isInstanceOf[DurableLike[_]]) throw new IllegalStateException()
    // XXX TODO --- ugly casts
    readIdentified1[Confluent.Txn, Durable.Txn](in)(tx.asInstanceOf[Durable.Txn]).asInstanceOf[Elem[T]]
  }

  private def readIdentified1[T <: KTxn[T], D1 <: Txn[D1]](in: DataInput)(implicit tx: D1): Cursors[T, D1] = {
    val targets = Targets.read[D1](in)
    val cookie  = in.readInt()
    if (cookie != COOKIE) sys.error(s"Unexpected $cookie (should be $COOKIE)")
    val seminal: Access[T] = confluent.Access.read(in) // system.readPath(in)
    val cursor  = confluent.Cursor.Data.read[T, D1](in)
    val name    = StringObj.readVar[D1](in)
    val list    = ListObj.Modifiable.read[D1, Cursors[T, D1] /* , Cursors.Update[T, D1] */](in)
    log(s"Cursors.read targets = $targets, list = $list")
    new Impl(targets, seminal, cursor, name, list)
  }

  private final class Impl[T <: KTxn[T], D1 <: Txn[D1]](
      protected val targets: Targets[D1], val seminal: Access[T] with Writable,
      val cursor: confluent.Cursor.Data[T, D1] with Disposable[D1] with Writable,
      nameVar: StringObj.Var[D1],
      list: ListObj.Modifiable[D1, Cursors[T, D1]]
    ) // (implicit tx: D1)
    extends Cursors[T, D1] with SingleEventNode[D1, Cursors.Update[T, D1]] {
    impl =>

    def tpe: Elem.Type = Cursors

    override def toString() = s"Cursors$id"

    def copy[Out <: Txn[Out]]()(implicit tx: D1, txOut: Out, context: Copy[D1, Out]): Elem[Out] = {
      type ListAux[~ <: Txn[~]] = ListObj.Modifiable[~, Cursors[T, ~]]
      if (tx != txOut) throw new UnsupportedOperationException(s"Cannot copy cursors across systems")
      // thus, we can now assume that D1 == Out, specifically that Out <: DurableLike[Out]
      val out = new Impl[T, D1](Targets[D1](), seminal, Data(cursor.path()),
        context(nameVar).asInstanceOf[StringObj.Var[D1]],
        context[ListAux](list).asInstanceOf[ListAux[D1]]
      ).connect()
      out.asInstanceOf[Elem[Out] /* Impl[T, Out] */]
    }

    def name(implicit tx: D1): StringObj[D1] = nameVar()
    def name_=(value: StringObj[D1])(implicit tx: D1): Unit = nameVar() = value

    def descendants(implicit tx: D1): Iterator[Cursors[T, D1]] = list.iterator

    def addChild(seminal: Access[T])(implicit tx: D1): Cursors[T, D1] = {
      val child = CursorsImpl[T, D1](seminal)
      log(s"$this.addChild($child)")
      list.addLast(child)
      child
    }

    def removeChild(child: Cursors[T, D1])(implicit tx: D1): Unit =
      if (!list.remove(child)) println(s"WARNING: Cursor $child was not a child of $impl")

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      seminal.write(out)
      cursor .write(out)
      nameVar.write(out)
      list   .write(out)
    }

    protected def disposeData()(implicit tx: D1): Unit = {
      disconnect()
      cursor .dispose()
      nameVar.dispose()
      list   .dispose()
    }

    //    private object GeneratorEvent
    //      extends impl.TriggerImpl[D1, Cursors.Change[T, D1], Cursors[T, D1]]
    //      with impl.EventImpl     [D1, Cursors.Change[T, D1], Cursors[T, D1]]
    //      with InvariantEvent     [D1, Cursors.Change[T, D1], Cursors[T, D1]]
    //      with impl.Root          [D1, Cursors.Change[T, D1]] {
    //
    //      protected def reader: Reader[D1, Cursors[T, D1]] = format
    //
    //      override def toString() = node.toString + ".GeneratorEvent"
    //      final val slot = 0
    //      def node = impl
    //    }

    def connect()(implicit tx: D1): this.type = {
      // log(s"$this.connect")
      // GeneratorEvent ---> this
      list.changed    ---> changed
      nameVar.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: D1): Unit = {
      // log(s"$this.disconnect()")
      // GeneratorEvent -/-> this
      list.changed    -/-> changed
      nameVar.changed -/-> changed
    }

    object changed extends Changed {
      override def toString = s"$node.changed"

      def pullUpdate(pull: Pull[D1])(implicit tx: D1): Option[Cursors.Update[T, D1]] = {
        val listEvt = list   .changed
        val nameEvt = nameVar.changed
        // val genOpt  = if (pull.contains(GeneratorEvent)) pull(GeneratorEvent) else None

        val nameOpt = if (pull.contains(nameEvt)) pull(nameEvt) else None
        // XXX TODO -- what the heck was this? : `Thread.sleep(50)`
        val listOpt = if (pull.contains(listEvt)) pull(listEvt) else None

        // println(s"---- enter pull : list = $listOpt")

        // val flat1   = genOpt.toIndexedSeq
        val flat1   = nameOpt.map(Cursors.Renamed[T, D1]).toIndexedSeq
        val changes = listOpt match {
          case Some(listUpd) =>
            val childUpdates = listUpd.changes.collect {
// ELEM
//              case expr.List.Element(child, childUpd) => Cursors.ChildUpdate (childUpd)
              case ListObj.Added  (idx, child)      => Cursors.ChildAdded  (idx, child)
              case ListObj.Removed(idx, child)      => Cursors.ChildRemoved(idx, child)
            }
            flat1 ++ childUpdates

          case _ => flat1
        }

        // println(s"---- exit pull : changes = $changes")

        if (changes.isEmpty) None else Some(Cursors.Update(impl, changes))
      }
    }
  }
}