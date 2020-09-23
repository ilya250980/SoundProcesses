/*
 *  Code.scala
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

import de.sciss.lucre.Event.Targets
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.lucre.{Ident, Txn, Var => LVar}
import de.sciss.serial.{ConstFormat, DataInput, DataOutput, Writable}
import de.sciss.{lucre, synth}
import de.sciss.synth.proc.impl.{CodeImpl => Impl}
import de.sciss.synth.proc.{Action => _Action, Control => _Control}

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}
import scala.concurrent.{ExecutionContext, Future}

object Code {
  final val typeId = 0x20001

  def init(): Unit = {
    Obj       .init()
    SynthGraph.init()
    Control   .init()
    Action    .init()
  }

  final val UserPackage = "user"

  /** Generates the default package statement. */
  def packagePrelude: String = s"package $UserPackage\n"

  final case class CompilationFailed() extends Exception
  final case class CodeIncomplete   () extends Exception

  object Import {
    sealed trait Selector {
      def sourceString: String
    }
    sealed trait Simple extends Selector
    case object Wildcard extends Simple {
      def sourceString = "_"
    }
    sealed trait Named extends Selector {
      /** Name under which the import is known in this source. */
      def name: String
    }
    final case class Name(name: String) extends Named with Simple {
      def sourceString: String = name
    }
    final case class Rename(from: String, to: String) extends Named {
      def name        : String = to
      def sourceString: String = s"$from => $to"
    }
    final case class Ignore(name: String) extends Selector {
      def sourceString: String = s"$name => _"
    }

    val All: List[Selector] = Wildcard :: Nil
  }
  final case class Import(prefix: String, selectors: List[Import.Selector]) {
    require (selectors.nonEmpty)

    /** The full expression, such as `scala.collection.immutable.{IndexedSeq => Vec}` */
    def expr: String = selectors match {
//      case Nil                            => prefix
      case (single: Import.Simple) :: Nil => s"$prefix.${single.sourceString}"
      case _                              => selectors.iterator.map(_.sourceString).mkString(s"$prefix.{", ", ", "}")
    }

    /** The equivalent source code, such as `import scala.collection.immutable.{IndexedSeq => Vec}` */
    def sourceString: String = s"import $expr"
  }

  implicit def format: ConstFormat[Code] = Impl.format

  def read(in: DataInput): Code = format.read(in)

  def future[A](fun: => A)(implicit compiler: Code.Compiler): Future[A] = Impl.future(fun)

  def registerImports(id: Int, imports: Seq[Import]): Unit = Impl.registerImports(id, imports)

  def getImports(id: Int): Vec[Import] = Impl.getImports(id)

  /** Generates the import statements prelude for a given code object. */
  def importsPrelude(code: Code, indent: Int = 0): String = Impl.importsPrelude(code, indent = indent)

  /** Generates the full prelude of a code object, containing package, imports, and code specific prelude. */
  def fullPrelude(code: Code): String =
    s"${Code.packagePrelude}${Code.importsPrelude(code)}${code.prelude}"

  // ---- type ----

  def apply(id: Int, source: String): Code = Impl(id, source)

  def addType(tpe: Type): Unit = Impl.addType(tpe)

  def getType(id: Int): Code.Type = Impl.getType(id)

  def types: ISeq[Code.Type] = Impl.types

  case class Example(name: String, mnemonic: Char, code: String)

  trait Type {
    def id: Int

    def prefix        : String
    def humanName     : String
    def docBaseSymbol : String

    /** Default source code to paste for new objects. */
    def defaultSource : String =
      s"// $humanName source code\n"

    def examples: ISeq[Example] = Nil

    type Repr <: Code

    private[this] lazy val _init: Unit = Code.addType(this)

    def init(): Unit = _init

    def mkCode(source: String): Repr
  }

  // ---- compiler ----

  def unpackJar(bytes: Array[Byte]): Map[String, Array[Byte]] = Impl.unpackJar(bytes)

  trait Compiler {
    implicit def executionContext: ExecutionContext

    /** Synchronous call to compile a source code consisting of a body which is wrapped in a `Function0` apply method,
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
    def interpret(source: String, print: Boolean, execute: Boolean): Any
  }

  // ---- type: SynthGraph ----

  object SynthGraph extends Type {
    final val id        = 1

    final val prefix    = "Proc"
    final val humanName = "Synth Graph"

    override def examples: ISeq[Example] = List(
      Example("Direct Out", 'd',
        """val n = WhiteNoise.ar("amp".ar(0.25))
          |val sig = SplayAz.ar(2, n)
          |Out.ar(0, sig)
          |""".stripMargin
      ),
      Example("Filter", 'f',
        """val in = ScanIn()
          |val sig = in
          |ScanOut(sig)
          |""".stripMargin
      ),
      Example("Analog Bubbles", 'a',
        """// James McCartney, SuperCollider 2
          |val pitch = LFSaw.kr(0.4)                // LFO
          |  .mulAdd(24, LFSaw.kr(List(8, 7.23))    // ... creating
          |  .mulAdd(3, 80))                        // ... a glissando
          |val osc = SinOsc.ar(pitch.midiCps) * 0.1 // sine wave
          |val verb = CombN.ar(osc, 0.2, 0.2, 4)    // echoing
          |Out.ar(0, verb)
          |""".stripMargin
      )
    )

    type Repr = SynthGraph

    def docBaseSymbol: String = "de.sciss.synth.ugen"

    def mkCode(source: String): Repr = SynthGraph(source)
  }
  final case class SynthGraph(source: String) extends Code {
    type In     = Unit
    type Out    = synth.SynthGraph

    def tpe: Code.Type = SynthGraph

    def compileBody()(implicit compiler: Code.Compiler): Future[Unit] = {
      import reflect.runtime.universe._
      Impl.compileBody[In, Out, Unit, SynthGraph](this, typeTag[Unit])
    }

    def execute(in: In)(implicit compiler: Code.Compiler): Out =
      synth.SynthGraph {
        import reflect.runtime.universe._
        Impl.compileThunk[Unit](this, typeTag[Unit], execute = true)
      }

    def prelude : String = "object Main {\n"

    def postlude: String = "\n}\n"

    def updateSource(newText: String): SynthGraph = copy(source = newText)
  }

  // ---- type: Control ----

  object Control extends Type {
    final val id        = 7
    final val prefix    = "Control"
    final val humanName = "Control Graph"
    type Repr           = Code

    override def examples: ISeq[Example] = List(
      Example("Hello World", 'h',
        """val b = LoadBang()
          |b ---> PrintLn("Hello World!")
          |""".stripMargin
      )
    )

    def docBaseSymbol: String = "de.sciss.lucre.expr.graph"

    def mkCode(source: String): Repr = Control(source)
  }
  final case class Control(source: String) extends Code {
    type In     = Unit
    type Out    = _Control.Graph

    def tpe: Type = Control

    def compileBody()(implicit compiler: Code.Compiler): Future[Unit] = {
      import reflect.runtime.universe._
      Impl.compileBody[In, Out, Unit, Control](this, typeTag[Unit])
    }

    def execute(in: In)(implicit compiler: Code.Compiler): Out =
      _Control.Graph {
        import reflect.runtime.universe._
        Impl.compileThunk[Unit](this, typeTag[Unit], execute = true)
      }

    def prelude : String = "object Main {\n"

    def postlude: String = "\n}\n"

    def updateSource(newText: String): Control = copy(source = newText)
  }

  // ---- type: Action ----

  object Action extends Type {
    final val id        = 8
    final val prefix    = "Action"
    final val humanName = "Action Graph"
    type Repr           = Action

    override def examples: ISeq[Example] = List(
      Example("Hello World", 'h',
        """PrintLn("Hello World!")
          |""".stripMargin
      )
    )

    override def defaultSource: String = s"${super.defaultSource}Act.Nop()\n"

    def docBaseSymbol: String = "de.sciss.lucre.expr.graph"

    def mkCode(source: String): Repr = Action(source)
  }
  final case class Action(source: String) extends Code {
    type In     = Unit
    type Out    = _Action.Graph

    def tpe: Type = Action

    def compileBody()(implicit compiler: Code.Compiler): Future[Unit] = {
      import reflect.runtime.universe._
      Impl.compileBody[In, Out, Act, Action](this, typeTag[Act])
    }

    def execute(in: In)(implicit compiler: Code.Compiler): Out =
      _Action.Graph {
        import reflect.runtime.universe._
        Impl.compileThunk[Act](this, typeTag[Act], execute = true)
      }

    def prelude : String =
      s"""object Main {
         |  def __result__ : ${classOf[Act].getName} = {
         |""".stripMargin

    def postlude: String = "\n  }\n}\n"

    def updateSource(newText: String): Action = copy(source = newText)
  }

  // ---- expr ----

  object Obj extends ExprTypeImpl[Code, Obj] {
    import Code.{Obj => Repr}

    def typeId: Int = Code.typeId

    def valueSerializer: ConstFormat[Code] = Code.format

    def tryParse(value: Any): Option[Code] = value match {
      case x: Code  => Some(x)
      case _        => None
    }

    protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
      new _Const[T](id, value)

    protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)(implicit tx: T): Var[T] = {
      val res = new _Var[T](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
      extends ConstImpl[T] with Repr[T]

    private final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
      extends VarImpl[T] with Repr[T]
  }
  trait Obj[T <: Txn[T]] extends lucre.Expr[T, Code]

  type T[I, O] = Code { type In = I; type Out = O }
}
trait Code extends Product with Writable { me =>
  type Self = Code.T[In, Out]

  /** The interfacing input type */
  type In
  /** The interfacing output type */
  type Out

  def tpe: Code.Type

  /** Source code. */
  def source: String

  /** Creates a new code object with updated source code. */
  def updateSource(newText: String): Self

  /** Generic source code prelude wrapping code,
    * containing package, class or object.
    * Should generally end in a newline.
    *
    * Must not include `Code.packagePrelude`.
    * Must not include imports as retrieved by `Code.importsPrelude`.
    */
  def prelude: String

  /** Source code postlude wrapping code,
    * containing for example closing braces.
    * Should generally begin and end in a newline.
    */
  def postlude: String

  /** Compiles the code body without executing it. */
  def compileBody()(implicit compiler: Code.Compiler): Future[Unit]

  /** Compiles and executes the code. Returns the wrapped result. */
  def execute(in: In)(implicit compiler: Code.Compiler): Out // = compile()(in)

  def write(out: DataOutput): Unit = Code.format.write(this, out)
}