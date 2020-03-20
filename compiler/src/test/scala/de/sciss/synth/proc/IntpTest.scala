package de.sciss.synth.proc

object IntpTest extends App{
  val c         = Compiler()
  val observed0 = c.interpret("1234", print = true, execute = true)
  println(s"observed0 = $observed0")
  val observed1 = c.interpret("5678", print = true, execute = true)
  println(s"observed1 = $observed1")
}
