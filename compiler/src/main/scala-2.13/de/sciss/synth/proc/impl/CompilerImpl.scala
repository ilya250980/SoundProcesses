/*
 *  CompilerImpl.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc
package impl

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileOutputStream}
import java.util.concurrent.Executors

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.tools.nsc
import scala.tools.nsc.interpreter.shell.ReplReporterImpl
import scala.tools.nsc.io.AbstractFile
import scala.tools.nsc.{ConsoleWriter, NewLinePrintWriter}
import scala.tools.nsc.interpreter.{IMain, Results}

object CompilerImpl {
  def apply(): Code.Compiler = new Impl({
    val cSet = new nsc.Settings()
    cSet.classpath.value += File.pathSeparator + sys.props("java.class.path")
    val c = new IMainImpl(cSet)
    // c.initializeSynchronous()
    c.initializeCompiler()
    c
  })

  private[impl] final class Impl(intp0: => IMain) extends Code.Compiler {
    private lazy val intp = intp0

    override def toString = s"Compiler@${hashCode().toHexString}"

    // note: the Scala compiler is _not_ reentrant!!
    implicit val executionContext: ExecutionContextExecutor =
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())

    def compile(source: String): Array[Byte] = {
      val compiler = intp.global  // we re-use the intp compiler -- no problem, right?
      intp.reset()
      compiler.reporter.reset()
      val f     = File.createTempFile("temp", ".scala")
      val out   = new BufferedOutputStream(new FileOutputStream(f))
      out.write(source.getBytes("UTF-8"))
      out.flush(); out.close()
      val run   = new compiler.Run()
      run.compile(List(f.getPath))
      f.delete()

      if (compiler.reporter.hasErrors) throw Code.CompilationFailed()

      val d0    = intp.replOutput.dir

      packJar(d0)
    }

    // cf. http://stackoverflow.com/questions/1281229/how-to-use-jaroutputstream-to-create-a-jar-file
    private def packJar(base: AbstractFile): Array[Byte] = {
      import java.util.jar._

      val mf = new Manifest
      mf.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      val bs    = new java.io.ByteArrayOutputStream
      val out   = new JarOutputStream(bs, mf)

      def add(prefix: String, f: AbstractFile): Unit = {
        val name0 = prefix + f.name // f.getName
        val name  = if (f.isDirectory) name0 + "/" else name0
        val entry = new JarEntry(name)
        entry.setTime(f.lastModified /* () */)
        // if (f.isFile) entry.setSize(f.length())
        out.putNextEntry(entry)
        if (!f.isDirectory /* f.isFile */) {
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

      base /* .listFiles() */.foreach(add("", _))
      out.close()
      bs.toByteArray
    }

    def interpret(source: String, print: Boolean, execute: Boolean): Any = {
      intp.reset()
//      val th  = Thread.currentThread()
//      val cl  = th.getContextClassLoader
      val res: Results.Result =
        if (print) {
          intp.interpret(source)
        } else {
          // Scala 2.13.1 for unknown reasons changed to `Unit` result in `beQuietDuring`
//            intp.beQuietDuring(intp.interpret(source))
          intp.reporter.withoutPrintingResults(intp.interpret(source))
        }

//      assert (th.getContextClassLoader == cl)

      // commented out to chase ClassNotFoundException
      // i.reset()
      res match {
        case Results.Success =>
          if (/* aTpe == "Unit" || */ !execute) () else {
            val n = intp.mostRecentVar
            intp.valueOfTerm(n).getOrElse(sys.error(s"No value for term $n"))
          }

        case Results.Error      => throw Code.CompilationFailed()
        case Results.Incomplete => throw Code.CodeIncomplete()
      }
    }
  }

  private final class IMainImpl(cSet: nsc.Settings)
    extends IMain(cSet, {
      val writer = new NewLinePrintWriter(new ConsoleWriter, autoFlush = true)
      new ReplReporterImpl(cSet, writer)
    }) {

    override protected def parentClassLoader: ClassLoader =
      CompilerImpl.getClass.getClassLoader
  }
}
