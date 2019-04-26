package de.sciss.lucre.expr

trait OscTest {

  val g = Graph {
    import graph._
    import ExOps._
    val n = OscUdpNode(40000)
    val m = OscUdpNode(40001)
    m.dump = 1

//    val value: Ex[Int] = 123

//    val act = n.send(m.self, OscMessage("/test", 123))
//    LoadBang() ---> act

    m.received ---> PrintLn(m.message.name)

    val ch    = Var[Int]()
    val relay = Var[Boolean]()
    val sel   = n.message.select("/iterate", ch, relay)

    val text  = Const("Match!") ++ ch.toStr

    sel ---> PrintLn(text)
  }
}
