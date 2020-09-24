///*
// *  ActionRawImpl.scala
// *  (SoundProcesses)
// *
// *  Copyright (c) 2010-2020 Hanns Holger Rutz. All rights reserved.
// *
// *	This software is published under the GNU Affero General Public License v3+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.synth.proc.impl
//
//import java.text.SimpleDateFormat
//import java.util.{Date, Locale}
//
//import de.sciss.lucre.event.Targets
//import de.sciss.lucre.stm.impl.ObjFormat
//import de.sciss.lucre.stm.{Copy, Cursor, Elem, IdPeek, NoSys, Obj, Sys, TxnLike, Workspace}
//import de.sciss.lucre.{stm, event => evt}
//import de.sciss.serial.{DataInput, DataOutput, TFormat}
//import de.sciss.synth.proc
//import de.sciss.synth.proc.{Action, ActionRaw, AuralSystem, Code, GenContext, Scheduler}
//
//import scala.annotation.switch
//import scala.collection.mutable
//import scala.concurrent.stm.{InTxn, TMap}
//import scala.concurrent.{Future, Promise, blocking}
//
//object ActionRawImpl {
//  private final val CONST_EMPTY   = 0
//  private final val CONST_JAR     = 1
//  private final val CONST_VAR     = 2
//  private final val CONST_BODY    = 3
//
//  private final val DEBUG         = false
//
//  // ---- creation ----
//
//  def mkName[T <: Txn[T]]()(implicit tx: T): String = {
//    val id = tx.newId()
//    s"Action${IdPeek(id)}"
//  }
//
//  def compile[T <: Txn[T]](source: Code.ActionRaw)
//                          (implicit tx: T, cursor: Cursor[T],
//                           compiler: Code.Compiler): Future[stm.Source[T, ActionRaw[T]]] = {
//    val name    = mkName[T]()
//    val p       = Promise[stm.Source[T, ActionRaw[T]]]()
//    val system  = tx.system
//    tx.afterCommit(performCompile(p, name, source, system))
//    p.future
//  }
//
//  def empty[T <: Txn[T]](implicit tx: T): ActionRaw[T] = new ConstEmptyImpl[T](tx.newId())
//
//  def newVar[T <: Txn[T]](init: ActionRaw[T])(implicit tx: T): ActionRaw.Var[T] = {
//    val targets = evt.Targets[T]
//    val peer    = tx.newVar(targets.id, init)
//    new VarImpl[T](targets, peer)
//  }
//
//  def newConst[T <: Txn[T]](name: String, jar: Array[Byte])(implicit tx: T): ActionRaw[T] =
//    new ConstJarImpl(tx.newId(), name, jar)
//
//  private val mapPredef = TMap.empty[String, Action.Body]
//
//  def predef[T <: Txn[T]](actionId: String)(implicit tx: T): ActionRaw[T] = {
//    if (!mapPredef.contains(actionId)(tx.peer))
//      throw new IllegalArgumentException(s"Predefined action '$actionId' is not registered")
//
//    new ConstBodyImpl[T](tx.newId(), actionId)
//  }
//
//  def registerPredef(actionId: String, body: Action.Body)(implicit tx: TxnLike): Unit =
//    if (mapPredef.put(actionId, body)(tx.peer).nonEmpty)
//      throw new IllegalArgumentException(s"Predefined action '$actionId' was already registered")
//
//  private def classLoader[T <: Txn[T]](implicit tx: T): MemoryClassLoader = sync.synchronized {
//    clMap.getOrElseUpdate(tx.system, {
//      if (DEBUG) println("ActionImpl: Create new class loader")
//      new MemoryClassLoader
//    })
//  }
//
//  def execute[T <: Txn[T]](universe: Action.Universe[T], name: String, jar: Array[Byte])(implicit tx: T): Unit = {
//    implicit val itx: InTxn = tx.peer
//    val cl = classLoader[T]
//    cl.add(name, jar)
//    val fullName  = s"${Code.UserPackage}.$name"
//    val clazz     = Class.forName(fullName, true, cl)
//    //  println("Instantiating...")
//    val fun = clazz.newInstance().asInstanceOf[Action.Body]
//    fun(universe)
//  }
//
//  // ----
//
//  private def performCompile[T <: Txn[T]](p: Promise[stm.Source[T, ActionRaw[T]]], name: String,
//                                          source: Code.ActionRaw, system: S)
//                                         (implicit cursor: Cursor[T], compiler: Code.Compiler): Unit = {
//    // val jarFut = source.compileToFunction(name)
//    val jarFut = Code.future(blocking(source.execute(name)))
//
//    // somehow we get problems with BDB on the compiler context.
//    // for simplicity use the main SP context!
//
//    // import compiler.executionContext
//    import de.sciss.synth.proc.SoundProcesses.executionContext
//    val actFut = jarFut.map { jar =>
//      if (DEBUG) println(s"ActionImpl: compileToFunction completed. jar-size = ${jar.length}")
//      cursor.step { implicit tx =>
//        val a = newConst(name, jar)
//        // Is this affected by https://github.com/Sciss/LucreConfluent/issues/6 ?
//        // No, as it doesn't contain any mutable state or Ident[T] instances
//        tx.newHandle(a)
//      }
//    }
//    p.completeWith(actFut)
//  }
//
//  // ---- universe ----
//
//  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'action' - ", Locale.US)
//
//  final class UniverseImpl[T <: Txn[T]](val self: ActionRaw[T],
//                                        val invoker: Option[Obj[T]], val value: Any)
//                                       (implicit val peer: proc.Universe[T])
//    extends Action.Universe[T] {
//
//    implicit def cursor     : Cursor    [T] = peer.cursor
//    implicit def workspace  : Workspace [T] = peer.workspace
//    implicit def genContext : GenContext[T] = peer.genContext
//    implicit val scheduler  : Scheduler [T] = peer.scheduler
//
//    def auralSystem: AuralSystem = peer.auralSystem
//
//    def log(what: => String)(implicit tx: T): Unit = tx.afterCommit {
//      Console.out.println(logHeader.format(new Date()) + what)
//    }
//  }
//
//  // ---- serialization ----
//
//  def format[T <: Txn[T]]: TFormat[T, ActionRaw[T]] = anySer.asInstanceOf[Ser[T]]
//
//  def varFormat[T <: Txn[T]]: TFormat[T, ActionRaw.Var[T]] = anyVarSer.asInstanceOf[VarSer[T]]
//
//  private val anySer    = new Ser   [NoSys]
//  private val anyVarSer = new VarSer[NoSys]
//
//  private final class Ser[T <: Txn[T]] extends ObjFormat[T, ActionRaw[T]] {
//    def tpe: Obj.Type = ActionRaw
//  }
//
//  def readIdentifiedObj[T <: Txn[T]](in: DataInput, access: S#Acc)(implicit tx: T): ActionRaw[T] =
//    in.readByte() match {
//      case 0 =>
//        val targets = Targets.readIdentified(in, access)
//        in.readByte() match {
//          case CONST_VAR =>
//            readIdentifiedVar(in, access, targets)
//          case other => sys.error(s"Unexpected action cookie $other")
//        }
//
//      case 3 =>
//        val id = tx.readId(in, access)
//        (in.readByte(): @switch) match {
//          case CONST_JAR    =>
//            val name    = in.readUTF()
//            val jarSize = in.readInt()
//            val jar     = new Array[Byte](jarSize)
//            in.readFully(jar)
//            // val system  = tx.system
//            new ConstJarImpl[T](id, name, jar)
//
//          case CONST_BODY   =>
//            val actionId = in.readUTF()
//            new ConstBodyImpl[T](id, actionId)
//
//          case CONST_EMPTY  => new ConstEmptyImpl[T](id)
//
//          case other => sys.error(s"Unexpected action cookie $other")
//        }
//    }
//
//  private final class VarSer[T <: Txn[T]] extends ObjFormat[T, ActionRaw.Var[T]] {
//    def tpe: Obj.Type = ActionRaw
//  }
//
//  private def readIdentifiedVar[T <: Txn[T]](in: DataInput, access: S#Acc, targets: evt.Targets[T])
//                                  (implicit tx: T): ActionRaw.Var[T] = {
//    val peer = tx.readVar[ActionRaw[T]](targets.id, in)
//    new VarImpl[T](targets, peer)
//  }
//
//  // ---- constant implementation ----
//
//  private val sync = new AnyRef
//
//  // this is why workspace should have a general caching system
//  private val clMap = new mutable.WeakHashMap[Sys[_], MemoryClassLoader]
//
//  private sealed trait ConstImpl[T <: Txn[T]] extends ActionRaw[T] with evt.impl.ConstObjImpl[T, Unit] {
//    final def tpe: Obj.Type = ActionRaw
//  }
//
//  private final class ConstBodyImpl[T <: Txn[T]](val id: Ident[T], val actionId: String)
//    extends ConstImpl[T] {
//
//    def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
//      new ConstBodyImpl(txOut.newId(), actionId) // .connect()
//
//    def execute(universe: Action.Universe[T])(implicit tx: T): Unit = {
//      implicit val itx: InTxn = tx.peer
//      val fun = mapPredef.getOrElse(actionId, sys.error(s"Predefined action '$actionId' not registered"))
//      fun(universe)
//    }
//
//    protected def writeData(out: DataOutput): Unit = {
//      out.writeByte(CONST_BODY)
//      out.writeUTF(actionId)
//    }
//  }
//
//  private final class ConstJarImpl[T <: Txn[T]](val id: Ident[T], val name: String, jar: Array[Byte])
//    extends ConstImpl[T] {
//
//    def execute(universe: Action.Universe[T])(implicit tx: T): Unit = {
//      ActionRawImpl.execute[T](universe, name, jar)
//    }
//
//    def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
//      new ConstJarImpl(txOut.newId(), name, jar) // .connect()
//
//    protected def writeData(out: DataOutput): Unit = {
//      out.writeByte(CONST_JAR)
//      out.writeUTF(name)
//      out.writeInt(jar.length)
//      out.write(jar)
//    }
//
//    override def hashCode(): Int = name.hashCode
//
//    override def equals(that: Any): Boolean = that match {
//      case cf: ConstJarImpl[_] => cf.name == name
//      case _ => super.equals(that)
//    }
//  }
//
//  private final class ConstEmptyImpl[T <: Txn[T]](val id: Ident[T]) extends ConstImpl[T] {
//    def execute(universe: Action.Universe[T])(implicit tx: T): Unit = ()
//
//    def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
//      new ConstEmptyImpl(txOut.newId()) // .connect()
//
//    protected def writeData(out: DataOutput): Unit =
//      out.writeByte(CONST_EMPTY)
//  }
//
//  private final class VarImpl[T <: Txn[T]](protected val targets: evt.Targets[T], peer: S#Var[ActionRaw[T]])
//    extends ActionRaw.Var[T]
//    with evt.impl.SingleNode[T, Unit] {
//
//    def tpe: Obj.Type = ActionRaw
//
//    def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] = {
//      def newTgt  = Targets[Out]
//      val newVr   = txOut.newVar[ActionRaw[Out]](newTgt.id, context(peer()))
//      new VarImpl[Out](newTgt, newVr) // .connect()
//    }
//
//    def apply()(implicit tx: T): ActionRaw[T] = peer()
//
//    def update(value: ActionRaw[T])(implicit tx: T): Unit = {
//      val old = peer()
//      peer()  = value
//      if (old != value) changed.fire(())
//    }
//
//    def swap(value: ActionRaw[T])(implicit tx: T): ActionRaw[T] = {
//      val res = apply()
//      update(value)
//      res
//    }
//
//    object changed extends Changed with evt.impl.RootGenerator[T, Unit]
//
//    def execute(universe: Action.Universe[T])(implicit tx: T): Unit = peer().execute(universe)
//
//    protected def disposeData()(implicit tx: T): Unit = peer.dispose()
//
//    protected def writeData(out: DataOutput): Unit = {
//      out.writeByte(CONST_VAR)
//      peer.write(out)
//    }
//  }
//}