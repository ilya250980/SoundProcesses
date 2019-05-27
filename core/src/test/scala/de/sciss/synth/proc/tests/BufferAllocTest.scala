package de.sciss.synth.proc.tests

import de.sciss.lucre.synth.BlockAllocator

object BufferAllocTest extends App {
  val buffers = BlockAllocator("test", 1024)

  import concurrent.stm.atomic

  println("aqui")
  val b0 = atomic { implicit tx => buffers.alloc(1) }
  val b1 = atomic { implicit tx => buffers.alloc(1) }
  val b2 = atomic { implicit tx => buffers.alloc(1) }
  val b3 = atomic { implicit tx => buffers.alloc(1) }
  val (b4, b5) = atomic { implicit tx => buffers.alloc(1) -> buffers.alloc(1) }
  println("aqui")
}