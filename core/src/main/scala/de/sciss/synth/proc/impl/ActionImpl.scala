/*
 *  ActionImpl.scala
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

package de.sciss.synth.proc
package impl

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import de.sciss.lucre.event.Targets
import de.sciss.lucre.stm.impl.ObjSerializer
import de.sciss.lucre.stm.{Copy, Cursor, Elem, IdPeek, NoSys, Obj, Sys, TxnLike, WorkspaceHandle}
import de.sciss.lucre.{stm, event => evt}
import de.sciss.serial.{DataInput, DataOutput, Serializer}
import de.sciss.synth.proc

import scala.annotation.switch
import scala.collection.mutable
import scala.concurrent.stm.{InTxn, TMap}
import scala.concurrent.{Future, Promise, blocking}

object ActionImpl {
  private final val CONST_EMPTY   = 0
  private final val CONST_JAR     = 1
  private final val CONST_VAR     = 2
  private final val CONST_BODY    = 3

  private final val DEBUG         = false

  // ---- creation ----

  def mkName[S <: Sys[S]]()(implicit tx: S#Tx): String = {
    val id = tx.newId()
    s"Action${IdPeek(id)}"
  }

  def compile[S <: Sys[S]](source: Code.Action)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S],
                           compiler: Code.Compiler): Future[stm.Source[S#Tx, Action[S]]] = {
    val name    = mkName[S]()
    val p       = Promise[stm.Source[S#Tx, Action[S]]]()
    val system  = tx.system
    tx.afterCommit(performCompile(p, name, source, system))
    p.future
  }

  def empty[S <: Sys[S]](implicit tx: S#Tx): Action[S] = new ConstEmptyImpl[S](tx.newId())

  def newVar[S <: Sys[S]](init: Action[S])(implicit tx: S#Tx): Action.Var[S] = {
    val targets = evt.Targets[S]
    val peer    = tx.newVar(targets.id, init)
    new VarImpl[S](targets, peer)
  }

  def newConst[S <: Sys[S]](name: String, jar: Array[Byte])(implicit tx: S#Tx): Action[S] =
    new ConstFunImpl(tx.newId(), name, jar)

  private val mapPredef = TMap.empty[String, Action.Body]

  def predef[S <: Sys[S]](actionId: String)(implicit tx: S#Tx): Action[S] = {
    if (!mapPredef.contains(actionId)(tx.peer))
      throw new IllegalArgumentException(s"Predefined action '$actionId' is not registered")

    new ConstBodyImpl[S](tx.newId(), actionId)
  }

  def registerPredef(actionId: String, body: Action.Body)(implicit tx: TxnLike): Unit =
    if (mapPredef.put(actionId, body)(tx.peer).nonEmpty)
      throw new IllegalArgumentException(s"Predefined action '$actionId' was already registered")

  private def classLoader[S <: Sys[S]](implicit tx: S#Tx): MemoryClassLoader = sync.synchronized {
    clMap.getOrElseUpdate(tx.system, {
      if (DEBUG) println("ActionImpl: Create new class loader")
      new MemoryClassLoader
    })
  }

  def execute[S <: Sys[S]](universe: Action.Universe[S], name: String, jar: Array[Byte])(implicit tx: S#Tx): Unit = {
    implicit val itx: InTxn = tx.peer
    val cl = classLoader[S]
    cl.add(name, jar)
    val fullName  = s"${Code.UserPackage}.$name"
    val clazz     = Class.forName(fullName, true, cl)
    //  println("Instantiating...")
    val fun = clazz.newInstance().asInstanceOf[Action.Body]
    fun(universe)
  }

  // ----

  private def performCompile[S <: Sys[S]](p: Promise[stm.Source[S#Tx, Action[S]]], name: String,
                                          source: Code.Action, system: S)
                                         (implicit cursor: stm.Cursor[S], compiler: Code.Compiler): Unit = {
    // val jarFut = source.compileToFunction(name)
    val jarFut = Code.future(blocking(source.execute(name)))

    // somehow we get problems with BDB on the compiler context.
    // for simplicity use the main SP context!

    // import compiler.executionContext
    import SoundProcesses.executionContext
    val actFut = jarFut.map { jar =>
      if (DEBUG) println(s"ActionImpl: compileToFunction completed. jar-size = ${jar.length}")
      cursor.step { implicit tx =>
        val a = newConst(name, jar)
        // Is this affected by https://github.com/Sciss/LucreConfluent/issues/6 ?
        // No, as it doesn't contain any mutable state or S#Id instances
        tx.newHandle(a)
      }
    }
    p.completeWith(actFut)
  }

  // ---- universe ----

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'action' - ", Locale.US)

  final class UniverseImpl[S <: Sys[S]](val self: Action[S],
                                         val invoker: Option[Obj[S]], val value: Any)
                                        (implicit peer: proc.Universe[S])
    extends Action.Universe[S] {

    implicit def cursor     : Cursor          [S] = peer.cursor
    implicit def workspace  : WorkspaceHandle [S] = peer.workspace
    implicit def genContext : GenContext      [S] = peer.genContext
    implicit val scheduler  : Scheduler       [S] = peer.scheduler

    def mkChild(newAuralSystem: AuralSystem, newScheduler: Scheduler[S]): Universe[S] = {
      val newPeer = peer.mkChild(newAuralSystem, newScheduler)
      new UniverseImpl[S](self = self, invoker = invoker, value = value)(peer = newPeer)
    }

    def auralSystem: AuralSystem = peer.auralSystem

    def mkRunner(obj: Obj[S])(implicit tx: S#Tx): Option[Runner[S]] = peer.mkRunner(obj)

    def runners(implicit tx: S#Tx): Iterator[Runner[S]] = peer.runners

    def log(what: => String)(implicit tx: S#Tx): Unit = tx.afterCommit {
      Console.out.println(logHeader.format(new Date()) + what)
    }

//    def mkTransport()(implicit tx: S#Tx): Transport[S] = Transport[S](this)
  }

  // ---- serialization ----

  def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action[S]] = anySer.asInstanceOf[Ser[S]]

  def varSerializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Action.Var[S]] = anyVarSer.asInstanceOf[VarSer[S]]

  private val anySer    = new Ser   [NoSys]
  private val anyVarSer = new VarSer[NoSys]

  private final class Ser[S <: Sys[S]] extends ObjSerializer[S, Action[S]] {
    def tpe: Obj.Type = Action
  }

  def readIdentifiedObj[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action[S] =
    in.readByte() match {
      case 0 =>
        val targets = Targets.readIdentified(in, access)
        in.readByte() match {
          case CONST_VAR =>
            readIdentifiedVar(in, access, targets)
          case other => sys.error(s"Unexpected action cookie $other")
        }

      case 3 =>
        val id = tx.readId(in, access)
        (in.readByte(): @switch) match {
          case CONST_JAR    =>
            val name    = in.readUTF()
            val jarSize = in.readInt()
            val jar     = new Array[Byte](jarSize)
            in.readFully(jar)
            // val system  = tx.system
            new ConstFunImpl[S](id, name, jar)

          case CONST_BODY   =>
            val actionId = in.readUTF()
            new ConstBodyImpl[S](id, actionId)

          case CONST_EMPTY  => new ConstEmptyImpl[S](id)

          case other => sys.error(s"Unexpected action cookie $other")
        }
    }

  private final class VarSer[S <: Sys[S]] extends ObjSerializer[S, Action.Var[S]] {
    def tpe: Obj.Type = Action
  }

  private def readIdentifiedVar[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                  (implicit tx: S#Tx): Action.Var[S] = {
    val peer = tx.readVar[Action[S]](targets.id, in)
    new VarImpl[S](targets, peer)
  }

  // ---- constant implementation ----

  private val sync = new AnyRef

  // this is why workspace should have a general caching system
  private val clMap = new mutable.WeakHashMap[Sys[_], MemoryClassLoader]

  private sealed trait ConstImpl[S <: Sys[S]] extends Action[S] with evt.impl.ConstObjImpl[S, Unit] {
    final def tpe: Obj.Type = Action
  }

  private final class ConstBodyImpl[S <: Sys[S]](val id: S#Id, val actionId: String)
    extends ConstImpl[S] {

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new ConstBodyImpl(txOut.newId(), actionId) // .connect()

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = {
      implicit val itx: InTxn = tx.peer
      val fun = mapPredef.getOrElse(actionId, sys.error(s"Predefined action '$actionId' not registered"))
      fun(universe)
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(CONST_BODY)
      out.writeUTF(actionId)
    }
  }

  // XXX TODO - should be called ConstJarImpl in next major version
  private final class ConstFunImpl[S <: Sys[S]](val id: S#Id, val name: String, jar: Array[Byte])
    extends ConstImpl[S] {

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = {
      ActionImpl.execute[S](universe, name, jar)
    }

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new ConstFunImpl(txOut.newId(), name, jar) // .connect()

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(CONST_JAR)
      out.writeUTF(name)
      out.writeInt(jar.length)
      out.write(jar)
    }

    override def hashCode(): Int = name.hashCode

    override def equals(that: Any): Boolean = that match {
      case cf: ConstFunImpl[_] => cf.name == name
      case _ => super.equals(that)
    }
  }

  private final class ConstEmptyImpl[S <: Sys[S]](val id: S#Id) extends ConstImpl[S] {
    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = ()

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
      new ConstEmptyImpl(txOut.newId()) // .connect()

    protected def writeData(out: DataOutput): Unit =
      out.writeByte(CONST_EMPTY)
  }

  private final class VarImpl[S <: Sys[S]](protected val targets: evt.Targets[S], peer: S#Var[Action[S]])
    extends Action.Var[S]
    with evt.impl.SingleNode[S, Unit] {

    def tpe: Obj.Type = Action

    def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] = {
      def newTgt  = Targets[Out]
      val newVr   = txOut.newVar[Action[Out]](newTgt.id, context(peer()))
      new VarImpl[Out](newTgt, newVr) // .connect()
    }

    def apply()(implicit tx: S#Tx): Action[S] = peer()

    def update(value: Action[S])(implicit tx: S#Tx): Unit = {
      val old = peer()
      peer()  = value
      if (old != value) changed.fire(())
    }

    def swap(value: Action[S])(implicit tx: S#Tx): Action[S] = {
      val res = apply()
      update(value)
      res
    }

    object changed extends Changed with evt.impl.RootGenerator[S, Unit]

    def execute(universe: Action.Universe[S])(implicit tx: S#Tx): Unit = peer().execute(universe)

    protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeByte(CONST_VAR)
      peer.write(out)
    }
  }
}