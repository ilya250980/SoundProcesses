/*
 *  Code.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2016 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import java.io.File

import de.sciss.lucre.event.Targets
import de.sciss.lucre.expr
import de.sciss.lucre.expr.impl.ExprTypeImpl
import de.sciss.lucre.stm.Sys
import de.sciss.processor.ProcessorLike
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer, Writable}
import de.sciss.synth
import de.sciss.synth.proc.impl.{CodeImpl => Impl}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.{ExecutionContext, Future, blocking}

object Code {
  final val typeID = 0x20001

  def init(): Unit = {
    Obj.init()
    FileTransform.init()
    SynthGraph   .init()
    Action       .init()
  }

  final val UserPackage = "user"

  final case class CompilationFailed() extends Exception
  final case class CodeIncomplete   () extends Exception

  implicit def serializer: ImmutableSerializer[Code] = Impl.serializer

  def read(in: DataInput): Code = serializer.read(in)

  def future[A](fun: => A)(implicit compiler: Code.Compiler): Future[A] = Impl.future(fun)

  def registerImports(id: Int, imports: Seq[String]): Unit = Impl.registerImports(id, imports)

  def getImports(id: Int): Vec[String] = Impl.getImports(id)

  // ---- type ----

  def apply(id: Int, source: String): Code = Impl(id, source)

  def addType(tpe: Type): Unit = Impl.addType(tpe)

  def getType(id: Int): Code.Type = Impl.getType(id)

  trait Type {
    def id: Int

    private[this] lazy val _init: Unit = Code.addType(this)

    def init(): Unit = _init

    def mkCode(source: String): Code
  }

  // ---- compiler ----

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = Impl.unpackJar(bytes)

  trait Compiler {
    implicit def executionContext: ExecutionContext

    /** Synchronous call to cmpile a source code consisting of a body which is wrapped in a `Function0` apply method,
      * returning the raw jar file produced in the compilation.
      *
      * May throw `CompilationFailed` or `CodeIncomplete`
      *
      * @param  source  the completely formatted source code to compile which should contain
      *                 a proper package and class definition. It must contain any
      *                 necessary `import` statements.
      * @return the jar file as byte-array, containing the opaque contents of the source code
      *         (possible the one single class defined)
      */
    def compile(source: String): Array[Byte]

    /** Synchronous call to compile and execute the provided source code.
      *
      * May throw `CompilationFailed` or `CodeIncomplete`
      *
      * @param  source  the completely formatted source code to compile which forms the body
      *                 of an imported object. It must contain any necessary `import` statements.
      * @return the evaluation result, or `()` if there is no result value
      */
    def interpret(source: String, execute: Boolean): Any
  }

  // ---- type: FileTransform ----

  object FileTransform extends Type {
    final val id    = 0
    final val name  = "File Transform"

    def mkCode(source: String): Code = FileTransform(source)
  }
  final case class FileTransform(source: String) extends Code {
    type In     = (File, File, ProcessorLike[Any, Any] => Unit)
    type Out    = Future[Unit]
    def id      = FileTransform.id

    def compileBody()(implicit compiler: Code.Compiler): Future[Unit] = Impl.compileBody[In, Out, FileTransform](this)

    def execute(in: In)(implicit compiler: Code.Compiler): Out = Impl.execute[In, Out, FileTransform](this, in)

    def contextName = FileTransform.name

    def updateSource(newText: String) = copy(source = newText)
  }

  // ---- type: SynthGraph ----

  object SynthGraph extends Type {
    final val id    = 1
    final val name  = "Synth Graph"

    def mkCode(source: String): Code = SynthGraph(source)
  }
  final case class SynthGraph(source: String) extends Code {
    type In     = Unit
    type Out    = synth.SynthGraph
    def id      = SynthGraph.id

    def compileBody()(implicit compiler: Code.Compiler): Future[Unit] = Impl.compileBody[In, Out, SynthGraph](this)

    def execute(in: In)(implicit compiler: Code.Compiler): Out = Impl.execute[In, Out, SynthGraph](this, in)

    def contextName = SynthGraph.name

    def updateSource(newText: String) = copy(source = newText)
  }

  // ---- type: Action ----

  object Action extends Type {
    final val id    = 2
    final val name  = "Action"

    def mkCode(source: String): Code = SynthGraph(source)
  }
  final case class Action(source: String) extends Code {
    type In     = String
    type Out    = Array[Byte]
    def id      = Action.id

    def compileBody()(implicit compiler: Code.Compiler): Future[Unit] = future(blocking { execute("Unnamed"); () })

    def execute(in: In)(implicit compiler: Code.Compiler): Out = {
      // Impl.execute[In, Out, Action](this, in)
      Impl.compileToFunction(in, this)
    }

    def contextName = Action.name

    def updateSource(newText: String) = copy(source = newText)

    // def compileToFunction(name: String): Future[Array[Byte]] = Impl.compileToFunction(name, this)
  }

  // ---- expr ----

  object Obj extends ExprTypeImpl[Code, Obj] {
    import Code.{Obj => Repr}

    def typeID = Code.typeID

    def valueSerializer: ImmutableSerializer[Code] = Code.serializer

    protected def mkConst[S <: Sys[S]](id: S#ID, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[Ex[S]], connect: Boolean)(implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[S <: Sys[S]](val id: S#ID, val constValue: A)
      extends ConstImpl[S] with Repr[S]

    private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[Ex[S]])
      extends VarImpl[S] with Repr[S]
  }
  trait Obj[S <: Sys[S]] extends expr.Expr[S, Code]
}
trait Code extends Writable { me =>
  type Self = Code { type In = me.In; type Out = me.Out }

  /** The interfacing input type */
  type In
  /** The interfacing output type */
  type Out

  /** Identifier to distinguish types of code. */
  def id: Int

  /** Source code. */
  def source: String

  def updateSource(newText: String): Self

  /** Human readable name. */
  def contextName: String

  /** Compiles the code body without executing it. */
  def compileBody()(implicit compiler: Code.Compiler): Future[Unit]

  /** Compiles and executes the code. Returns the wrapped result. */
  def execute(in: In)(implicit compiler: Code.Compiler): Out // = compile()(in)

  def write(out: DataOutput): Unit = Code.serializer.write(this, out)
}