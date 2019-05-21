/*
 *  CodeImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc.impl

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.proc.Code
import de.sciss.synth.proc.Code.Import

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.concurrent.{Future, blocking}

object CodeImpl {
  private final val COOKIE  = 0x436F6465  // "Code"

  // ---- type ----

  @volatile private var map = Map.empty[Int, Code.Type]

  def addType(tpe: Code.Type): Unit = sync.synchronized {
    val typeId = tpe.id
    if (map.contains(typeId))
      throw new IllegalArgumentException(s"Code type $typeId was already registered ($tpe overrides ${map(typeId)})")

    map += typeId -> tpe
  }

  def getType(id: Int): Code.Type = map.getOrElse(id, sys.error(s"Unknown element type $id"))

  def apply(id: Int, source: String): Code = getType(id).mkCode(source)

  def types: ISeq[Code.Type] = map.valuesIterator.toList.sortBy(_.humanName)

  // ----

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = {
    // cf. http://stackoverflow.com/questions/8909743/
    import java.util.jar._

    val in = new JarInputStream(new ByteArrayInputStream(bytes))
    val b  = Map.newBuilder[String, Array[Byte]]

    while ({
      val entry = in.getNextJarEntry
      (entry != null) && {
        if (!entry.isDirectory) {
          val name = entry.getName
          val bs  = new ByteArrayOutputStream
          val arr = new Array[Byte](1024)
          while ({
            val sz = in.read(arr, 0, 1024)
            (sz > 0) && { bs.write(arr, 0, sz); true }
          }) ()
          val bytes = bs.toByteArray
          b += mkClassName(name) -> bytes
        }
        true
      }
    }) ()

    in.close()
    b.result()
  }

  /* Converts a jar entry name with slashes to a class name with dots
   * and dropping the `class` extension
   */
  private def mkClassName(path: String): String = {
    require(path.endsWith(".class"))
    path.substring(0, path.length - 6).replace("/", ".")
  }

  implicit object serializer extends ImmutableSerializer[Code] {
    def write(v: Code, out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeInt(v.tpe.id)
      out.writeUTF(v.source)
    }

    def read(in: DataInput): Code = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      val id      = in.readInt()
      val source  = in.readUTF()
      Code.apply(id, source)
    }
  }

  //  // note: the Scala compiler is _not_ reentrant!!
  //  private implicit val executionContext: ExecutionContextExecutor =
  //    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

  def future[A](fun: => A)(implicit compiler: Code.Compiler): Future[A] =
    concurrent.Future(fun)(compiler.executionContext)

  // ---- imports ----

  private val sync = new AnyRef

  import Import._

  private var importsMap = Map[Int, Vec[Import]](
    Code.SynthGraph.id -> Vec(
      Import("de.sciss.synth", List(Ignore("Buffer"), Wildcard)),
      Import("de.sciss.synth.ugen", List(Ignore("DiskIn"), Ignore("VDiskIn"), Ignore("BufChannels"),
        Ignore("BufRateScale"), Ignore("BufSampleRate"), Wildcard)),
      Import("de.sciss.synth.proc.graph", All),
      Import("de.sciss.synth.proc.graph.Ops", All)
    ),
    Code.Action.id -> Vec(     // what should go inside?
      Import("scala.util", List(Name("Try"), Name("Success"), Name("Failure"))),
      Import("de.sciss.file", All),
      Import("de.sciss.lucre.artifact", List(Name("Artifact"), Name("ArtifactLocation"))),
      Import("de.sciss.lucre.expr", List(Name("Expr"), Name("BooleanObj"), Name("IntObj"), Name("LongObj"),
        Name("DoubleObj"), Name("StringObj"), Name("IntVector"), Name("DoubleVector"), Name("SpanObj"),
        Name("SpanLikeObj"))),
      Import("de.sciss.lucre.expr.Ops", All),
      Import("de.sciss.lucre.stm", List(Name("Obj"), Name("Folder"))),
      Import("de.sciss.numbers.Implicits", All),
      Import("de.sciss", Name("osc") :: Nil),
      Import("de.sciss.span", All),
      Import("de.sciss.synth", Name("io") :: Nil),
      Import("de.sciss.synth.proc", All),
      Import("de.sciss.synth.proc.Implicits", All)
    )
  )

  def registerImports(id: Int, imports: Seq[Import]): Unit = sync.synchronized {
    importsMap += id -> importsMap.get(id).fold(imports.toIndexedSeq)(_ ++ imports)
  }

  def getImports(id: Int): Vec[Import] = importsMap(id)

  // ---- internals ----

//  final def execute[I, O, A, Repr <: Code { type In = I; type Out = O }](code: Repr, in: I)
//                                                                     (implicit w: Wrapper[I, O, A, Repr],
//                                                                      compiler: Code.Compiler): O = {
//    w.wrap(in) {
//      compileThunk(code, w, execute = true)
//    }
//  }

  def compileBody[I, O, A, Repr <: Code.T[I, O]](code: Repr, tt: reflect.runtime.universe.TypeTag[A])
                                                (implicit compiler: Code.Compiler): Future[Unit] =
    future {
      blocking {
        compileThunk[A](code, tt = tt, execute = false)
      }
    }

  /** Compiles a source code consisting of a body which is wrapped in its prelude/postlude,
    * and returns the raw jar file produced in the compilation.
    */
  def compileToJar(name: String, code: Code, prelude: String, postlude: String)
                  (implicit compiler: Code.Compiler): Array[Byte] = {
    val impS    = importsPrelude(code, indent = 2)
    val source  =
      s"${Code.packagePrelude}$impS$prelude${code.source}$postlude"

    // println(source)

    compiler.compile(source)
  }

  object Run {
    def apply[A](execute: Boolean)(thunk: => A): A = if (execute) thunk else null.asInstanceOf[A]
  }

  private val pkgCode = "de.sciss.synth.proc.impl.CodeImpl"

  def importsPrelude(code: Code, indent: Int = 0): String =
    importsMap(code.tpe.id).iterator.map(i => s"${"  " * indent}${i.sourceString}\n").mkString

  // note: synchronous.
  def compileThunk[A](code: Code, tt: reflect.runtime.universe.TypeTag[A], execute: Boolean)
                     (implicit compiler: Code.Compiler): A = {
    val impS    = importsPrelude(code, indent = 1)
    val aTpe    = tt.tpe.toString // not `clazz.getName` which gives `void` instead of `Unit`!
    val isUnit  = aTpe == "Unit"
    val synth =
      s"""$pkgCode.Run[$aTpe]($execute) {
        |$impS
        |
        |""".stripMargin + code.source + "\n}"

    val res: Any = compiler.interpret(synth, print = false, execute = execute && !isUnit)
    res.asInstanceOf[A]
  }
}