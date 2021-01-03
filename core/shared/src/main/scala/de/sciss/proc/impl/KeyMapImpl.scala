/*
 *  KeyMapImpl.scala
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

package de.sciss.proc.impl

import de.sciss.lucre.{Disposable, Txn}
import de.sciss.lucre.data.SkipList
import de.sciss.serial.{DataInput, DataOutput, TFormat, Writable}

object KeyMapImpl {
  trait ValueInfo[T <: Txn[T], Key, Value] {
    // def valueEvent(value: Value): EventLike[T, ValueUpd]

    def keyFormat  : TFormat[T, Key]
    def valueFormat: TFormat[T, Value]
  }

  implicit def entryFormat[T <: Txn[T], Key, Value](implicit info: ValueInfo[T, Key, Value])
  : TFormat[T, Entry[T, Key, Value]] = new EntrySer[T, Key, Value]

  private final class EntrySer[T <: Txn[T], Key, Value](implicit info: ValueInfo[T, Key, Value])
    extends TFormat[T, Entry[T, Key, Value]] {

    def write(e: Entry[T, Key, Value], out: DataOutput): Unit = e.write(out)

    def readT(in: DataInput)(implicit tx: T): Entry[T, Key, Value] = {
      val key   = info.keyFormat  .readT(in)
      val value = info.valueFormat.readT(in)
      new Entry[T, Key, Value](key, value)
    }
  }

  final class Entry[T <: Txn[T], Key, Value](val key: Key,
                                             val value: Value)(implicit info: ValueInfo[T, Key, Value])
    extends Writable with Disposable[T] {

//    def connect   ()(implicit tx: T): Unit = info.valueEvent(value) ---> this
//    def disconnect()(implicit tx: T): Unit = info.valueEvent(value) -/-> this

    def write(out: DataOutput): Unit = {
      info.keyFormat  .write(key  , out)
      info.valueFormat.write(value, out)
    }

    def dispose()(implicit tx: T): Unit = ()

//    def pullUpdate(pull: evt.Pull[T])(implicit tx: T): Option[(Key, ValueUpd)] =
//      pull(info.valueEvent(value)).map(key -> _)
  }
}

/** Common building block for implementing reactive maps where the key is a constant element
  * (that is, it does not require updating such as an `Ident[T]`).
  *
  * @tparam Key       the type of key, such as `String`
  * @tparam Value     the value type, which has an event attached to it (found via `valueInfo`)
  */
trait KeyMapImpl[T <: Txn[T], Key, Value] {
  // _: evt.VirtualNodeSelector[T] =>
  // _: evt.impl.MappingNode[T] with evt.Publisher[T, ]

  protected type Entry = KeyMapImpl.Entry    [T, Key, Value]
  protected type Info  = KeyMapImpl.ValueInfo[T, Key, Value]

  /** The underlying non-reactive map */
  protected def map: SkipList.Map[T, Key, Entry]

  /** Wrap the given set of added and removed keys in an appropriate update message
    * and dispatch it.
    */
  protected def fire(added: Option[(Key, Value)], removed: Option[(Key, Value)])(implicit tx: T): Unit

  /** A helper object providing key and value serialization and an event view of the value. */
  protected implicit def valueInfo: Info

  final def get(key: Key)(implicit tx: T): Option[Value] = map.get(key).map(_.value)

  final def keys(implicit tx: T): Set[Key] = map.keysIterator.toSet

  final def iterator(implicit tx: T): Iterator[(Key, Value)] =
    map.iterator.map {
      case (key, entry) => key -> entry.value
    }

  final def add(key: Key, value: Value)(implicit tx: T): Unit = {
    val n = new KeyMapImpl.Entry[T, Key, Value](key, value)
    val optRemoved: Option[(Key, Value)] = map.put(key, n).map { oldNode =>
      // this -= oldNode
      key -> oldNode.value
    }
    // this += n
    fire(added = Some(key -> value), removed = optRemoved)
  }

  final def remove(key: Key)(implicit tx: T): Boolean =
    map.remove(key).exists { oldNode =>
      // this -= oldNode
      fire(added = None, removed = Some(key -> oldNode.value))
      true
    }
}
