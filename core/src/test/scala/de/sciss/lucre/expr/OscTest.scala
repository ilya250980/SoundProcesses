package de.sciss.lucre.expr

trait OscTest {

  val g = Graph {
    import graph._
    import ExOps._
    val n = OscUdpNode(40000)
    val m = OscUdpNode(40001)
    m.dump = 1

    val act = n.send(m.self, OscMessage("/test", 123))
//    LoadBang() ---> act
  }
}
