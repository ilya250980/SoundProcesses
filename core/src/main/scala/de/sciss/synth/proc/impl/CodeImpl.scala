/*
 *  CodeImpl.scala
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

package de.sciss.synth.proc.impl

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.proc.Code

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.{Future, blocking}
import scala.reflect.ClassTag

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

  // ----

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = {
    import java.util.jar._

    import scala.annotation.tailrec

    val in = new JarInputStream(new ByteArrayInputStream(bytes))
    val b  = Map.newBuilder[String, Array[Byte]]

    @tailrec def loop(): Unit = {
      val entry = in.getNextJarEntry
      if (entry != null) {
        if (!entry.isDirectory) {
          val name  = entry.getName

          // cf. http://stackoverflow.com/questions/8909743/jarentry-getsize-is-returning-1-when-the-jar-files-is-opened-as-inputstream-f
          val bs  = new ByteArrayOutputStream
          var i   = 0
          while (i >= 0) {
            i = in.read()
            if (i >= 0) bs.write(i)
          }
          val bytes = bs.toByteArray
          b += mkClassName(name) -> bytes
        }
        loop()
      }
    }
    loop()
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
      out.writeInt(v.id)
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

  private var importsMap = Map[Int, Vec[String]](
    Code.SynthGraph.id -> Vec(
      "de.sciss.synth.{Buffer => _, _}",
      "de.sciss.synth.ugen.{DiskIn => _, VDiskIn => _, BufChannels => _, BufRateScale => _, BufSampleRate => _, _}",
      "de.sciss.synth.proc.graph._",
      "de.sciss.synth.proc.graph.Ops._"
    ),
    Code.Action.id -> Vec(     // what should go inside?
      "scala.util.{Try, Success, Failure}",
      "de.sciss.file._",
      "de.sciss.lucre.artifact.{Artifact, ArtifactLocation}",
      "de.sciss.lucre.expr.{Expr, BooleanObj, IntObj, LongObj, DoubleObj, StringObj, IntVector, DoubleVector, SpanObj, SpanLikeObj}",
      "de.sciss.lucre.expr.Ops._",
      "de.sciss.lucre.stm.{Obj, Folder}",
      "de.sciss.numbers.Implicits._",
      "de.sciss.osc",
      "de.sciss.span._",
      "de.sciss.synth.io",
      "de.sciss.synth.proc._",
      "de.sciss.synth.proc.Implicits._"
    )
  )

  def registerImports(id: Int, imports: Seq[String]): Unit = sync.synchronized {
    importsMap += id -> importsMap.get(id).fold(imports.toIndexedSeq)(_ ++ imports)
  }

  def getImports(id: Int): Vec[String] = importsMap(id)

  // ---- internals ----

//  final def execute[I, O, A, Repr <: Code { type In = I; type Out = O }](code: Repr, in: I)
//                                                                     (implicit w: Wrapper[I, O, A, Repr],
//                                                                      compiler: Code.Compiler): O = {
//    w.wrap(in) {
//      compileThunk(code, w, execute = true)
//    }
//  }

  def compileBody[I, O, A: ClassTag, Repr <: Code.T[I, O]](code: Repr)
                                                          (implicit compiler: Code.Compiler): Future[Unit] =
    future {
      blocking {
        compileThunk[A](code, execute = false)
      }
    }

  /** Compiles a source code consisting of a body which is wrapped in its prelude/postlude,
    * and returns the raw jar file produced in the compilation.
    */
  def compileToJar(name: String, code: Code.Action, prelude: String, postlude: String)
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
    importsMap(code.id).map(i => s"${"  " * indent}import $i\n").mkString

  // note: synchronous
  def compileThunk[A: ClassTag](code: Code, execute: Boolean)(implicit compiler: Code.Compiler): A = {
    val impS  = importsPrelude(code, indent = 1)
    val ct    = implicitly[ClassTag[A]].runtimeClass
    val aTpe  = ct.getName // w.returnType
    val synth =
      s"""$pkgCode.Run[$aTpe]($execute) {
        |$impS
        |
        |""".stripMargin + code + "\n}"

    val res: Any = compiler.interpret(synth, execute = execute && ct != classOf[Unit])
    res.asInstanceOf[A]
  }
}