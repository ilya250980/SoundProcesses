package de.sciss.synth.proc

import org.scalatest.funspec.AnyFunSpec

/*
  To run only this suite:

  testOnly de.sciss.synth.proc.Issue71

 */
class Issue71 extends AnyFunSpec {
  describe("Compiler") {
    it("should successively return correct results from `interpret`") {
      val c       = Compiler()
      val values  = Seq(1234, 5678)
      values.foreach { v =>
        val res = c.interpret(v.toString, print = false, execute = true)
        assert (res == v)
      }
    }
  }
}