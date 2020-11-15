package de.sciss.proc.impl

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileOutputStream}
import java.util.concurrent.Executors

import de.sciss.proc.Code

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.language.implicitConversions
import scala.reflect.internal.FatalError
import scala.tools.nsc
import scala.tools.nsc.interpreter.shell.ReplReporterImpl
import scala.tools.nsc.interpreter.{IMain, Results}
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter}

object CompilerImpl {
  def apply(): Code.Compiler = new Impl({
    val cSet = new nsc.Settings()
    cSet.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val c = new IMainImpl(cSet)
    c.initializeCompiler()
    c
  })

  private[impl] final class Impl(intp0: => IMain) extends Code.Compiler {
    private lazy val intp: IMain = intp0

    override def toString = s"Compiler@${hashCode().toHexString}"

    // note: the Scala compiler is _not_ reentrant!!
    implicit val executionContext: ExecutionContextExecutor =
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

    def compile(source: String): Array[Byte] = {
      val compiler = intp.global // we re-use the intp compiler -- no problem, right?
      intp.reset()
      compiler.reporter.reset()
      val f = File.createTempFile("temp", ".scala")
      val out = new BufferedOutputStream(new FileOutputStream(f))
      out.write(source.getBytes("UTF-8"))
      out.flush();
      out.close()
      val run = new compiler.Run()
      run.compile(List(f.getPath))
      f.delete()

      if (compiler.reporter.hasErrors) throw Code.CompilationFailed()

      val d0 = intp.replOutput.dir

      packJar(d0)
    }

    // cf. http://stackoverflow.com/questions/1281229/how-to-use-jaroutputstream-to-create-a-jar-file
    private def packJar(base: AbstractFile): Array[Byte] = {
      import java.util.jar._

      val mf = new Manifest
      mf.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      val bs = new java.io.ByteArrayOutputStream
      val out = new JarOutputStream(bs, mf)

      def add(prefix: String, f: AbstractFile): Unit = {
        val name0 = prefix + f.name // f.getName
        val name = if (f.isDirectory) name0 + "/" else name0
        val entry = new JarEntry(name)
        entry.setTime(f.lastModified /* () */)
        // if (f.isFile) entry.setSize(f.length())
        out.putNextEntry(entry)
        if (!f.isDirectory /* f.isFile */ ) {
          val in = new BufferedInputStream(f.input /* new FileInputStream(f) */)
          try {
            val buf = new Array[Byte](1024)

            @tailrec def loop(): Unit = {
              val count = in.read(buf)
              if (count >= 0) {
                out.write(buf, 0, count)
                loop()
              }
            }

            loop()
          } finally {
            in.close()
          }
        }
        out.closeEntry()
        if (f.isDirectory) f /* .listFiles */ .foreach(add(name, _))
      }

      base /* .listFiles() */ .foreach(add("", _))
      out.close()
      bs.toByteArray
    }

    def interpret(source: String, print: Boolean, execute: Boolean): Any = {
      val _intp = intp
      _intp.reset()
      val res: Results.Result =
        if (print) {
          _intp.interpret(source)
        } else {
          // Scala 2.13.1 for unknown reasons changed to `Unit` result in `beQuietDuring`
          //            intp.beQuietDuring(intp.interpret(source))
          _intp.reporter.withoutPrintingResults(_intp.interpret(source))
        }

      res match {
        case Results.Success =>
          if ( /* aTpe == "Unit" || */ !execute) () else {
            val n = _intp.mostRecentVar
            _intp.valueOfTerm(n).getOrElse(sys.error(s"No value for term $n"))
          }

        case Results.Error => throw Code.CompilationFailed()
        case Results.Incomplete => throw Code.CodeIncomplete()
      }
    }
  }

  private final class IMainImpl(cSet: nsc.Settings)
    extends IMain(cSet, {
      val writer = new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)
      new ReplReporterImpl(cSet, writer)
    }) {

    import global._

    import scala.reflect.runtime.{universe => ru}
    import scala.util.{Try => Trying}

    // XXX TODO --- this will unnecessary in Scala 2.13.2
    private[this] var _runtimeMirror: ru.Mirror = null

    private def runtimeMirrorFix: ru.Mirror = {
      if (_runtimeMirror == null) {
        _runtimeMirror = ru.runtimeMirror(classLoader)
      }
      _runtimeMirror
    }

    // XXX TODO --- this will unnecessary in Scala 2.13.2
    override def resetClassLoader(): Unit = {
      super.resetClassLoader()
      _runtimeMirror = null
    }

    // this is private in super
    private lazy val importToGlobal = global mkImporter ru

    private implicit def importFromRu(sym: ru.Symbol): Symbol = importToGlobal importSymbol sym

    // XXX TODO --- this will be fixed in Scala 2.13.2
    override lazy val runtimeMirror: ru.Mirror =
      throw new UnsupportedOperationException("Disabled due to Scala issue 11915")

    private def noFatal(body: => Symbol): Symbol = try body catch {
      case _: FatalError => NoSymbol
    }

    // XXX TODO --- this will unnecessary in Scala 2.13.2
    override def getClassIfDefined(path: String): Symbol = (
      noFatal(runtimeMirrorFix staticClass path)
        orElse noFatal(rootMirror staticClass path)
      )

    override def getModuleIfDefined(path: String): Symbol = (
      noFatal(runtimeMirrorFix staticModule path)
        orElse noFatal(rootMirror staticModule path)
      )

    // we have to override this to insert a fresh `runtimeMirror`
    // to work around https://github.com/scala/bug/issues/11915
    // except for `val runtimeMirror`, to code remains the same.
    // XXX TODO --- this will be fixed in Scala 2.13.2
    override def valueOfTerm(id: String): Option[Any] = {
      def value(fullName: String) = {
        val runtimeMirror = runtimeMirrorFix // !!!
        import runtimeMirror.universe.{InstanceMirror, Symbol, TermName}
        val pkg :: rest = (fullName split '.').toList
        val top = runtimeMirror.staticPackage(pkg)

        @annotation.tailrec
        def loop(inst: InstanceMirror, cur: Symbol, path: List[String]): Option[Any] = {
          def mirrored =
            if (inst != null) inst
            else {
              val res = runtimeMirror.reflect((runtimeMirror reflectModule cur.asModule).instance)
              res
            }

          path match {
            case last :: Nil =>
              val decls = cur.typeSignature.decls
              val declOpt = decls.find { x =>
                x.name.toString == last && x.isAccessor
              }
              declOpt.map { m =>
                val mm = m.asMethod
                val mr = mirrored.reflectMethod(mm)
                mr.apply()
              }
            case next :: rest =>
              val s = cur.typeSignature.member(TermName(next))
              val i =
                if (s.isModule) {
                  if (inst == null) null
                  else runtimeMirror.reflect((inst reflectModule s.asModule).instance)
                }
                else if (s.isAccessor) {
                  runtimeMirror.reflect(mirrored.reflectMethod(s.asMethod).apply())
                }
                else {
                  assert(false, originalPath(s))
                  inst
                }
              loop(i, s, rest)
            case Nil => None
          }
        }

        loop(null, top, rest)
      }

      Option(symbolOfTerm(id)).filter(_.exists).flatMap(s => Trying(value(originalPath(s))).toOption.flatten)
    }

    override protected def parentClassLoader: ClassLoader =
      CompilerImpl.getClass.getClassLoader
  }

}
