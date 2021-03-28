/*
 *  CodeImpl.scala
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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}
import de.sciss.proc.Code
import de.sciss.proc.Code.Import
import de.sciss.proc.legacy.ActionRaw

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

  def types: ISeq[Code.Type] = (map - ActionRaw.Code.id).valuesIterator.toList.sortBy(_.humanName)

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

  implicit object format extends ConstFormat[Code] {
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
    Code.Proc.id -> Vec(
      Import("de.sciss.synth", List(Ignore("Buffer"), Wildcard)),
      Import("de.sciss.synth.ugen", List(Ignore("DiskIn"), Ignore("DiskOut"), Ignore("VDiskIn"),
        Ignore("BufChannels"), Ignore("BufRateScale"), Ignore("BufSampleRate"), Wildcard)),
      Import("de.sciss.synth.proc.graph", All),
      Import("de.sciss.synth.proc.graph.Ops", All)
    ),
    Code.Control.id -> Vec(     // what should go inside?
      Import("de.sciss.numbers.Implicits"   , All),
      Import("de.sciss.proc.ExImport" , All),
      Import("de.sciss.lucre.expr.graph"    , All)
    ),
    Code.Action.id -> Vec(
      Import("de.sciss.numbers.Implicits"   , All),
      Import("de.sciss.proc.ExImport" , All),
      Import("de.sciss.lucre.expr.graph"    , All)
    ),
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

  def compileBody[I, O, A, Repr <: Code.T[I, O]](code: Repr, resCl: Class[A])
                                                (implicit compiler: Code.Compiler): Future[Unit] =
    future {
      blocking {
        compileThunk[A](code, resCl = resCl, execute = false)
      }
      ()
    }

  /** Compiles a source code consisting of a body which is wrapped in its prelude/postlude,
    * and returns the raw jar file produced in the compilation.
    *
    * @param  name  Note: it is currently unused
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

  private val pkgCode = "de.sciss.proc.impl.CodeImpl"

  def importsPrelude(code: Code, indent: Int = 0): String =
    importsMap(code.tpe.id).iterator.map(i => s"${"  " * indent}${i.sourceString}\n").mkString

  // note: synchronous.
  def compileThunk[A](code: Code, resCl: Class[A], execute: Boolean)
                     (implicit compiler: Code.Compiler): A = {
    val impS    = importsPrelude(code, indent = 1)
//    val aTpe    = tt.tpe.toString // not `clazz.getName` which gives `void` instead of `Unit`!
    val isUnit  = resCl == classOf[Unit]
    val resName = if (isUnit) "Unit" else {
      val n0 = resCl.getName
      val tp = resCl.getTypeParameters.length
      if (tp == 0) n0 else Seq.fill(tp)("_").mkString(s"$n0[", "_", "]")
    }
    val synth =
      s"""$pkgCode.Run[$resName]($execute) {
        |$impS
        |
        |""".stripMargin + code.source + "\n}"

    val res: Any = compiler.interpret(synth, print = false, execute = execute && !isUnit)
    res.asInstanceOf[A]
  }
}