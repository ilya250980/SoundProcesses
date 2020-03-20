package de.sciss.synth.proc.tests

import java.nio.ByteBuffer

import de.sciss.osc
import de.sciss.synth.proc.{Code, Compiler}

import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.Success

object ActionTest extends App {
  val PRINT_BYTECODE = false

  class MemoryClassLoader extends ClassLoader {
    private var map: Map[String, Array[Byte]] = Map.empty

    def += (entry: (String, Array[Byte])): Unit = {
      map += entry
    }

    def ++= (entries: Iterable[(String, Array[Byte])]): Unit = {
      map ++= entries
    }

    override protected def findClass(name: String): Class[_] =
      map.get(name).map { bytes =>
        ActionTest.synchronized {
          println(s"defineClass($name, ...)")
        }
        defineClass(name, bytes, 0, bytes.length)

      } .getOrElse(super.findClass(name)) // throws exception
  }

  val exampleSource =
    """val xs = List("hello", "world")
      |println(xs.map(_.capitalize).mkString(" "))
      |""".stripMargin

  implicit val compiler: Code.Compiler = Compiler()
  import ExecutionContext.Implicits.global

  iter("Foo").andThen {
    case Success(_) => iter("Bar").andThen {
      case Success(bytes) =>
        val map = Code.unpackJar(bytes)
        val cl = new MemoryClassLoader
        cl ++= map
        val clName  = s"${Code.UserPackage}.Bar"
        println(s"Resolving class '$clName'...")
        val clazz = Class.forName(clName, true, cl)
        println("Instantiating...")
        val x     = clazz.newInstance().asInstanceOf[() => Unit]
        println("Invoking 'apply':")
        ActionTest.synchronized {
          x()
        }
        sys.exit()
    }
  }

  def iter(name: String): Future[Array[Byte]] = {
    val code  = Code.ActionRaw(exampleSource)
    println("Compiling...")
//    val fut   = Code.future(blocking(CodeImpl.compileToJar(name, code)))
    val fut   = Code.future(blocking(code.execute(name)))

    fut.onComplete { res =>
      println("Result:")
      println(res)
      res match {
        case Success(bytes) =>
          val map = Code.unpackJar(bytes)
          // map.keys.foreach(println)
          map.foreach { case (key, value) =>
            ActionTest.synchronized {
              println(s"class: '$key', size: ${value.length}, hash: ${value.toVector.hashCode().toHexString}")
              val i = key.lastIndexOf('.')
              if (PRINT_BYTECODE && key.substring(i + 1) == name) {
                val bb = ByteBuffer.wrap(value)
                osc.Packet.printHexOn(bb, System.out)
              }
            }
          }

        case _ =>
      }
    }

    fut // .map(_ => ())
  }
}
