/*
 *  CursorsImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2018 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import de.sciss.lucre.confluent.Cursor.Data
import de.sciss.lucre.confluent.{Sys => KSys}
import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr.StringObj
import de.sciss.lucre.stm.{Copy, Elem, Sys, DurableLike => DSys}
import de.sciss.lucre.{confluent, expr, stm, event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer, Writable}

object CursorsImpl {
  private final val COOKIE = 0x43737273 // "Csrs"

  def apply[S <: KSys[S], D1 <: DSys[D1]](seminal: S#Acc)
                                         (implicit tx: D1#Tx): Cursors[S, D1] = {
    val targets = evt.Targets[D1]
    val cursor  = confluent.Cursor.Data[S, D1](seminal)
    val name    = StringObj.newVar[D1]("branch")
    type CursorAux[~ <: stm.Sys[~]] = Cursors[S, ~]
    val list    = expr.List.Modifiable[D1, CursorAux]
    log(s"Cursors.apply targets = $targets, list = $list")
    new Impl(targets, seminal, cursor, name, list).connect()
  }

  implicit def serializer[S <: KSys[S], D1 <: DSys[D1]]:
    Serializer[D1#Tx, D1#Acc, Cursors[S, D1]] = new Ser[S, D1]

  private final class Ser[S <: KSys[S], D1 <: DSys[D1]]
    extends Serializer[D1#Tx, D1#Acc, Cursors[S, D1]] {

    def write(v: Cursors[S, D1], out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: Unit)(implicit tx: D1#Tx): Cursors[S, D1] = {
      val tpe     = in.readInt()
      if (tpe != Cursors.typeID) sys.error(s"Type mismatch, found $tpe, expected ${Cursors.typeID}")
      readIdentified1[S, D1](in, access)
    }
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elem[S] = {
    if (!tx.system.isInstanceOf[stm.DurableLike[_]]) throw new IllegalStateException()
    // XXX TODO --- ugly casts
    readIdentified1[Confluent, Durable](in, ())(tx.asInstanceOf[Durable#Tx]).asInstanceOf[Elem[S]]
  }

  private def readIdentified1[S <: KSys[S], D1 <: DSys[D1]](in: DataInput, access: Unit)
                                                           (implicit tx: D1#Tx): Cursors[S, D1] = {
    val targets = Targets.read[D1](in, access)
    val cookie  = in.readInt()
    if (cookie != COOKIE) sys.error(s"Unexpected $cookie (should be $COOKIE)")
    val seminal: S#Acc = confluent.Access.read(in) // system.readPath(in)
    val cursor  = confluent.Cursor.Data.read[S, D1](in, access)
    val name    = StringObj.readVar[D1](in, access)
    val list    = expr.List.Modifiable.read[D1, Cursors[S, D1] /* , Cursors.Update[S, D1] */](in, access)
    log(s"Cursors.read targets = $targets, list = $list")
    new Impl(targets, seminal, cursor, name, list)
  }

  private final class Impl[S <: KSys[S], D1 <: DSys[D1]](
      protected val targets: evt.Targets[D1], val seminal: S#Acc with Writable,
      val cursor: confluent.Cursor.Data[S, D1] with stm.Disposable[D1#Tx] with Writable,
      nameVar: StringObj.Var[D1],
      list: expr.List.Modifiable[D1, Cursors[S, D1]]
    ) // (implicit tx: D1#Tx)
    extends Cursors[S, D1] with evt.impl.SingleNode[D1, Cursors.Update[S, D1]] {
    impl =>

    def tpe: Elem.Type = Cursors

    override def toString() = s"Cursors$id"

    def copy[Out <: Sys[Out]]()(implicit tx: D1#Tx, txOut: Out#Tx, context: Copy[D1, Out]): Elem[Out] = {
      type ListAux[~ <: Sys[~]] = expr.List.Modifiable[~, Cursors[S, ~]]
      if (tx != txOut) throw new UnsupportedOperationException(s"Cannot copy cursors across systems")
      // thus, we can now assume that D1 == Out, specifically that Out <: DurableLike[Out]
      val out = new Impl[S, D1](Targets[D1], seminal, Data(cursor.path()),
        context(nameVar).asInstanceOf[StringObj.Var[D1]],
        context[ListAux](list).asInstanceOf[ListAux[D1]]
      ).connect()
      out.asInstanceOf[Elem[Out] /* Impl[S, Out] */]
    }

    def name(implicit tx: D1#Tx): StringObj[D1] = nameVar()
    def name_=(value: StringObj[D1])(implicit tx: D1#Tx): Unit = nameVar() = value

    def descendants(implicit tx: D1#Tx): Iterator[Cursors[S, D1]] = list.iterator

    def addChild(seminal: S#Acc)(implicit tx: D1#Tx): Cursors[S, D1] = {
      val child = CursorsImpl[S, D1](seminal)
      log(s"$this.addChild($child)")
      list.addLast(child)
      child
    }

    def removeChild(child: Cursors[S, D1])(implicit tx: D1#Tx): Unit =
      if (!list.remove(child)) println(s"WARNING: Cursor $child was not a child of $impl")

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      seminal.write(out)
      cursor .write(out)
      nameVar.write(out)
      list   .write(out)
    }

    protected def disposeData()(implicit tx: D1#Tx): Unit = {
      disconnect()
      cursor .dispose()
      nameVar.dispose()
      list   .dispose()
    }

    //    private object GeneratorEvent
    //      extends evt.impl.TriggerImpl[D1, Cursors.Change[S, D1], Cursors[S, D1]]
    //      with evt.impl.EventImpl     [D1, Cursors.Change[S, D1], Cursors[S, D1]]
    //      with evt.InvariantEvent     [D1, Cursors.Change[S, D1], Cursors[S, D1]]
    //      with evt.impl.Root          [D1, Cursors.Change[S, D1]] {
    //
    //      protected def reader: evt.Reader[D1, Cursors[S, D1]] = serializer
    //
    //      override def toString() = node.toString + ".GeneratorEvent"
    //      final val slot = 0
    //      def node = impl
    //    }

    def connect()(implicit tx: D1#Tx): this.type = {
      // log(s"$this.connect")
      // GeneratorEvent ---> this
      list.changed    ---> changed
      nameVar.changed ---> changed
      this
    }

    private[this] def disconnect()(implicit tx: D1#Tx): Unit = {
      // log(s"$this.disconnect()")
      // GeneratorEvent -/-> this
      list.changed    -/-> changed
      nameVar.changed -/-> changed
    }

    object changed extends Changed {
      override def toString = s"$node.changed"

      def pullUpdate(pull: evt.Pull[D1])(implicit tx: D1#Tx): Option[Cursors.Update[S, D1]] = {
        val listEvt = list   .changed
        val nameEvt = nameVar.changed
        // val genOpt  = if (pull.contains(GeneratorEvent)) pull(GeneratorEvent) else None

        val nameOpt = if (pull.contains(nameEvt)) pull(nameEvt) else None
        // XXX TODO -- what the heck was this? : `Thread.sleep(50)`
        val listOpt = if (pull.contains(listEvt)) pull(listEvt) else None

        // println(s"---- enter pull : list = $listOpt")

        // val flat1   = genOpt.toIndexedSeq
        val flat1   = nameOpt.map(Cursors.Renamed[S, D1]).toIndexedSeq
        val changes = listOpt match {
          case Some(listUpd) =>
            val childUpdates = listUpd.changes.collect {
// ELEM
//              case expr.List.Element(child, childUpd) => Cursors.ChildUpdate (childUpd)
              case expr.List.Added  (idx, child)      => Cursors.ChildAdded  (idx, child)
              case expr.List.Removed(idx, child)      => Cursors.ChildRemoved(idx, child)
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