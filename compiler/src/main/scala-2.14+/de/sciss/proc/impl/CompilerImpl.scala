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

package de.sciss.proc
package impl

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import dotty.tools.repl.{Command, MyRendering, MyReplDriver, ParseResult, Parsed, SigKill, State, SyntaxErrors}

import java.io.{OutputStream, PrintStream, Writer}
import scala.util.{Failure, Success, Try}

// XXX TODO this is a quick hack
object CompilerImpl {
  def apply(): Code.Compiler = {
    val out     = Console.out // config.out.fold(Console.out)(wr => new PrintStream(new WriterOutputStream(wr)))
    val loader  = getClass.getClassLoader
    new Impl(out, loader)
  }

//  private final class WriterOutputStream(wr: Writer) extends OutputStream {
//    override def write(b: Array[Byte], off: Int, len: Int): Unit = {
//      val str = new String(b, off, len)
//      wr.write(str, 0, str.length)
//    }
//
//    def write(b: Int): Unit = write(Array(b.toByte), 0, 1)
//  }
  
  private final val REPL_SESSION_LINE  = "rs$line$"
  private final val REPL_RES_PREFIX    = "res"
  
  private final case class Incomplete() extends Exception

  private[impl] final class Impl(out: PrintStream, loader: ClassLoader) extends Code.Compiler {
    private lazy val driver = new MyReplDriver(
      Array("-usejavacp", "-color:never", "-Xrepl-disable-display"), out, Some(loader)
    )
//    private lazy val rendering = new MyRendering(Some(getClass.getClassLoader))

    // note: the Scala compiler is _not_ reentrant!!
    implicit val executionContext: ExecutionContextExecutor =
      ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor())
    
    def compile(source: String): Array[Byte] = ???
    
//    private[this] var valIndex    = 0
//    private[this] var objectIndex = 0

    def interpret(source: String, print: Boolean, execute: Boolean): Any = {
      driver.resetToInitial() // this resets the class loader and thus "forgets" previous results
      // therefore, we can start from zero counters for the variables
      val state0  = driver.initialState // .copy(valIndex = valIndex, objectIndex = objectIndex)
//      println(s"state0 $state0")
      
//      state = driver.run(source)(state)
      val parsed0: ParseResult = ParseResult(source)(using state0)
      val state: State = parsed0 match {
        case parsed: Parsed if parsed.trees.nonEmpty =>
          driver.compile(parsed, state0) match {
            case Left(errs) =>
              driver.displayErrors(errs)(using state0)
              throw Code.CompilationFailed()

            case Right(state1) => state1
          }

        case SyntaxErrors(_, errs, _) =>
          driver.displayErrors(errs)(using state0)
          throw Code.CompilationFailed()

//        case cmd: Command =>
//          interpretCommand(cmd)
//
//        case SigKill => // TODO
//          state

        case _ => // new line, empty tree
          state0
      }

      //      throw Code.CodeIncomplete()
//      throw Code.CompilationFailed()
      
      // import dotc.core.StdNames.str
      val oid = state .objectIndex
      val vid = state0.valIndex
//      objectIndex = state.objectIndex + 1  // or + 0 ?
//      valIndex    = state.valIndex    + 1  // or + 0 ?
      
//      println(s"new state $state")
      
      if (!execute) () else {
        val rendering = driver.getRendering
        val cl        = rendering.classLoader()(using state.context)
        val clazz     = Class.forName(s"${/* str. */REPL_SESSION_LINE}$oid", true, cl)
        val methodOpt = clazz.getDeclaredMethods.find(_.getName == s"${/* str. */REPL_RES_PREFIX}$vid")
        val valueOpt: Option[Any] = methodOpt
          .map(_.invoke(null))
        val value = valueOpt.getOrElse(())
        val methodName = methodOpt.fold("")(_.getName)
        if (print && methodOpt.isDefined && valueOpt.isDefined && !valueOpt.contains(())) {
          println(s"$methodName: $value")
        }
        value
      }
    }
  }
}
