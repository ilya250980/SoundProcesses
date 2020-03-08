/*
 *  BlockAllocatorImpl.scala
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

package de.sciss.lucre.synth.impl

import de.sciss.lucre.synth.{BlockAllocator, logAlloc}

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap => ISortedMap}
import scala.concurrent.stm.{InTxn, Ref}

object BlockAllocatorImpl {
  /**
   * Creates a new block allocator with the given address range.
   *
   * @param start   the start address (inclusive)
   * @param stop    the stop address (exclusive)
   */
  def apply(name: String, stop: Int, start: Int = 0): BlockAllocator = {
    require(stop >= start, s"stop ($stop) must be greater than or equal to start ($start)")
    new Impl(name, start = start, stop = stop)
  }

  private final case class State(freeBySize: ISortedMap[Int, Set[Block]],
                                 freeByStart: ISortedMap[Int, Block],
                                 used: Set[Block])

  private final class Impl(name: String, start: Int, stop: Int) extends BlockAllocator {
    private val ref = Ref(State(
      freeBySize  = ISortedMap((stop - start) -> Set(Block(start, stop - start))),
      freeByStart = ISortedMap(start -> Block(start, stop - start)),
      used = Set.empty
    ))

    override def toString = s"BlockAllocator(name = $name, start = $start, stop = $stop)@${hashCode().toHexString}"

    def alloc(size: Int = 1)(implicit tx: InTxn): Int = {
      val res = findAvailable(size) match {
        case Some(bFree) =>
          val bRes = reserve(bFree, size)
          bRes.start
        case _ => -1
      }
      logAlloc(s"$name alloc $size -> $res @${tx.hashCode().toHexString}/${Thread.currentThread().hashCode().toHexString}")
      res
    }

    def free(address: Int, size: Int)(implicit tx: InTxn): Unit = {
      logAlloc(s"$name free $address, $size @${tx.hashCode().toHexString}/${Thread.currentThread().hashCode().toHexString}")
      val b       = Block(address, size)
      val state0  = ref()
      require(state0.used.contains(b), s"Freeing an unregistered block $b")
      val state1  = state0.copy(used = state0.used - b)

      @tailrec def merge(iter: Iterator[Block], b: Block, bRem: List[Block]): (Block, List[Block]) = {
        if (iter.hasNext) {
          val bn = iter.next()
          if (bn.touches(b)) {
            val bm = b.join(bn)
            merge(iter, bm, bn :: bRem)
          } else {
            (b, bRem)
          }
        } else {
          (b, bRem)
        }
      }

      val f             = state1.freeByStart
      val (bm1, bRem1)  = merge(f.from (address).valuesIterator, b  , Nil  )
      val (bm , bRem )  = merge(f.until(address).valuesIterator, bm1, bRem1)

      val state2 = bRem.foldLeft(state1) { case (s, b2) => removeFree(s, b2) }
      val state3 = addFree(state2, bm)
      ref() = state3
    }

    private def findAvailable(n: Int)(implicit tx: InTxn): Option[Block] = {
      val f = ref().freeBySize
      f.from(n).headOption.map(_._2.head)
    }

    private def addFree(state: State, b: Block): State = {
      state.copy(freeBySize = {
        val map = state.freeBySize
        map + (b.size -> (map.getOrElse(b.size, Set.empty) + b))
      }, freeByStart = state.freeByStart + (b.start -> b))
    }

    private def removeFree(state: State, b: Block): State = {
      state.copy(freeBySize = {
        val map     = state.freeBySize
        val newSet  = map.getOrElse(b.size, Set.empty) - b
        if (newSet.isEmpty) {
          map - b.size
        } else {
          map + (b.size -> newSet)
        }
      }, freeByStart = state.freeByStart - b.start)
    }

    private def reserve(block: Block, size: Int)(implicit tx: InTxn): Block = {
      assert(block.size >= size)
      val state0 = ref()
      val (res, newState) = if (block.size == size) {
        val state1 = removeFree(state0, block)
        val state2 = addToUsed(state1, block)
        block -> state2
      } else {
        val blockUsed = Block(block.start, size)
        val blockFree = Block(block.start + size, block.size - size)
        val state1 = removeFree(state0, block)
        val state2 = addToUsed(state1, blockUsed)
        val state3 = addFree(state2, blockFree)
        blockUsed -> state3
      }
      ref() = newState
      res
    }

    @inline private def addToUsed(state: State, b: Block): State =
      state.copy(used = state.used + b)

    def consistencyCheck()(implicit tx: InTxn): Unit = {
      val state = ref()
      val f = state.freeBySize
      val g = state.freeByStart
      val h = state.used

      for (f1 <- f; f2 <- f if f2 != f1; f11 <- f1._2; f22 <- f2._2) {
        require(!f11.touches(f22), f)
      }
      for (f1 <- f; f11 <- f1._2) {
        require(g.contains(f11.start), s"In freeBySize but not ...ByStart : $f11")
        require(!h.contains(f11), s"In freeBySize but not used : $f11")
      }
      for (g1 <- g) {
        val g1b = g1._2
        require(f.getOrElse(g1b.size, Set.empty).contains(g1b), s"In freeByStart but not ...BySize : $g1b")
        require(!h.contains(g1b), s"In freeByStart but not used : $g1b")
      }

      val all0  = (f.values.flatten ++ g.values ++ h).toSet.toSeq
      val all   = all0.sortBy(_.start)
      //         if( all.size > 1 ) {
      all.sliding(2, 1).foreach {
        seq => val a = seq.head; val b = seq.last; require(a.touches(b), s"$a does not touch $b")
      }
      //         }
      val one = all.reduce(_ join _)
      require(one == Block(start, stop - start), one)
    }
  }

  private final case class Block(start: Int, size: Int) {
    override def toString = s"Block(start = $start, size = $size)"

    def touches(b: Block): Boolean =
      ((  start <= b.start) && (  start +   size >= b.start)) ||
      ((b.start <=   start) && (b.start + b.size >=   start))

    def join(that: Block): Block = {
      val newStart = math.min(start, that.start)
      val newSize  = math.max(start + size, that.start + that.size) - newStart
      Block(newStart, newSize)
    }
  }
}