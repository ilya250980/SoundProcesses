/*
 *  Runner.scala
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

package de.sciss.lucre.expr.graph

import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IExprAsRunnerMap}
import de.sciss.lucre.impl.IChangeGeneratorEvent
import de.sciss.lucre.synth.AnyTxn
import de.sciss.lucre.{IChangeEvent, IExpr, IPull, ITargets, Txn, synth}
import de.sciss.model.Change
import de.sciss.proc
import de.sciss.proc.Universe
import de.sciss.proc.impl.MutableRunnerImpl

import scala.concurrent.stm.Ref

object Runner extends ProductReader[Runner] {
  private final class ExpandedRun[T <: Txn[T]](r: proc.Runner[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      r.run()
  }

  object Run extends ProductReader[Run] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Run = {
      assert (arity == 1 && adj == 0)
      val _r = in.readProductT[Runner]()
      new Run(_r)
    }
  }
  final case class Run(r: Runner) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Runner$$Run" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      new ExpandedRun[T](rx)
    }
  }

  private final class ExpandedRunWith[T <: Txn[T]](r: proc.Runner[T], attr: proc.Runner.Attr[T])
    extends IActionImpl[T] {

    def executeAction()(implicit tx: T): Unit = {
      r.prepare(attr)
      r.run()
    }
  }

  object RunWith extends ProductReader[RunWith] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): RunWith = {
      assert (arity == 2 && adj == 0)
      val _r    = in.readProductT[Runner]()
      val _map  = in.readVec(in.readEx[(String, _)]())
      new RunWith(_r, _map)
    }
  }
  final case class RunWith(r: Runner, map: Seq[Ex[(String, _)]]) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Runner$$RunWith" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx    = r.expand[T]
      val mapEx = map.map(_.expand[T])
      val attr  = new IExprAsRunnerMap[T](mapEx, tx)
      new ExpandedRunWith[T](rx, attr)
    }
  }

  private final class ExpandedStop[T <: Txn[T]](r: proc.Runner[T]) extends IActionImpl[T] {
    def executeAction()(implicit tx: T): Unit =
      r.stop()
  }

  object Stop extends ProductReader[Stop] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Stop = {
      assert (arity == 1 && adj == 0)
      val _r = in.readProductT[Runner]()
      new Stop(_r)
    }
  }
  final case class Stop(r: Runner) extends Act {
    type Repr[T <: Txn[T]] = IAction[T]

    override def productPrefix: String = s"Runner$$Stop" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val rx = r.expand[T]
      new ExpandedStop[T](rx)
    }
  }

  private final class ExpandedState[T <: Txn[T]](r: proc.Runner[T], tx0: T)
                                                (implicit protected val targets: ITargets[T])
    extends IExpr[T, Int] with IChangeGeneratorEvent[T, Int] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.react { implicit tx => state =>
      val now     = state.id
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Int = r.state.id

    def dispose()(implicit tx: T): Unit = obs.dispose()

    def changed: IChangeEvent[T, Int] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Int =
      pull.resolveExpr(this)
  }

  object State extends ProductReader[State] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): State = {
      assert (arity == 1 && adj == 0)
      val _r = in.readProductT[Runner]()
      new State(_r)
    }
  }
  final case class State(r: Runner) extends Ex[Int] {
    type Repr[T <: Txn[T]] = IExpr[T, Int]

    override def productPrefix: String = s"Runner$$State" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx = r.expand[T]
      new ExpandedState[T](rx, tx)
    }
  }

  private final class ExpandedProgress[T <: Txn[T]](r: proc.Runner[T], tx0: T)
                                                (implicit protected val targets: ITargets[T])
    extends IExpr[T, Double] with IChangeGeneratorEvent[T, Double] {

    private[this] val beforeRef = Ref(value(tx0))

    private[this] val obs = r.progress.react { implicit tx => now =>
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Double = r.progress.current

    def dispose()(implicit tx: T): Unit = obs.dispose()

    def changed: IChangeEvent[T, Double] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Double =
      pull.resolveExpr(this)
  }

  object Progress extends ProductReader[Progress] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Progress = {
      assert (arity == 1 && adj == 0)
      val _r = in.readProductT[Runner]()
      new Progress(_r)
    }
  }
  final case class Progress(r: Runner) extends Ex[Double] {
    type Repr[T <: Txn[T]] = IExpr[T, Double]

    override def productPrefix: String = s"Runner$$Progress" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx = r.expand[T]
      new ExpandedProgress[T](rx, tx)
    }
  }

  private type Message  = (Long, Int, String)
  private type Msg      = Seq[Message]

  private final class ExpandedMessages[T <: Txn[T]](r: proc.Runner[T], tx0: T)
                                                   (implicit protected val targets: ITargets[T])
    extends IExpr[T, Msg] with IChangeGeneratorEvent[T, Msg] {

    private[this] val beforeRef = Ref(value(tx0))

    private def msgOf(in: proc.Runner.Message): Message = (in.time, in.level.value, in.text)

    private[this] val obs = r.messages.react { implicit tx => now0 =>
      val now     = now0.map(msgOf)
      val before  = beforeRef.swap(now)(tx.peer)
      val ch      = Change(before, now)
      if (ch.isSignificant) fire(ch)
    } (tx0)

    def value(implicit tx: T): Msg = r.messages.current.map(msgOf)

    def dispose()(implicit tx: T): Unit = obs.dispose()

    def changed: IChangeEvent[T, Msg] = this

    private[lucre] def pullChange(pull: IPull[T])(implicit tx: T, phase: IPull.Phase): Msg =
      pull.resolveExpr(this)
  }

  object Messages extends ProductReader[Messages] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Messages = {
      assert (arity == 1 && adj == 0)
      val _r = in.readProductT[Runner]()
      new Messages(_r)
    }
  }
  final case class Messages(r: Runner) extends Ex[Seq[(Long, Int, String)]] {
    type Repr[T <: Txn[T]] = IExpr[T, Seq[(Long, Int, String)]]

    override def productPrefix: String = s"Runner$$Messages" // serialization

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      val rx = r.expand[T]
      new ExpandedMessages[T](rx, tx)
    }
  }

  /** Creates a runner from an attribute of a given static key. That is,
    * it looks up an object in the enclosing attribute-map, and creates
    * a runner representation for it, if it exists. Otherwise the runner
    * is "empty" and does not respond to `run`.
    */
  def apply(key: String): Runner = {
    // Impl(key)
    val ex = Attr.WithDefault[Obj](key, Obj.empty)
    apply(ex)
  }

  /** Creates a runner from a given object.
    * If the object has not yet been created, or there is no runner factory
    * available for it, the runner is "empty" and does not respond to `run`.
    */
  def apply(obj: Ex[Obj]): Runner = Impl(obj)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Runner = {
    assert (arity == 1 && adj == 0)
//    val _key = in.readString()
//    Runner(_key)
    in.readElem() match {
      case _ex : Ex[_]  => Runner(_ex.asInstanceOf[Ex[Obj]])
      case _key: String => Runner(_key) // legacy serialization
    }
  }

  private final class ExpandedImpl[T <: Txn[T]](objEx: IExpr[T, Obj], tx0: T)(implicit universe: Universe[T])
    extends MutableRunnerImpl[T](None, tx0) {

    private def setObj(obj: Obj)(implicit tx: T): Unit = {
      val peerOpt = obj.peer[T]
      // println(s"RUNNER setObj($peerOpt)")
      val rOpt = peerOpt.flatMap(pObj => proc.Runner.get(pObj))
      peer = rOpt
    }

    private[this] val objObs = objEx.changed.react { implicit tx => obj =>
      // println(s"objEx.changed - $obj")
      setObj(obj.now)
    } (tx0)

    setObj(objEx.value(tx0))(tx0)

    override def dispose()(implicit tx: T): Unit = {
      objObs.dispose()
      super .dispose()
    }
  }

  private final case class Impl(obj: Ex[Obj]) extends Runner {
    override def productPrefix: String = "Runner" // serialization

    type Repr[T <: Txn[T]] = proc.Runner[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] =
      tx match {
        case stx: synth.Txn[_] =>
          // ugly...
          val tup = (ctx, stx).asInstanceOf[(Context[AnyTxn], AnyTxn)]
          mkControlImpl(tup).asInstanceOf[Repr[T]]

        case _ => throw new Exception("Need a SoundProcesses system")
      }

    private def mkControlImpl[T <: synth.Txn[T]](tup: (Context[T], T)): Repr[T] = {
      implicit val ctx: Context[T]  = tup._1
      implicit val tx: T            = tup._2
      import ctx.{cursor, workspace}
      implicit val h: Universe[T] = Universe()
      new ExpandedImpl[T](obj.expand[T], tx)
    }
  }
}
trait Runner extends Control {
  type Repr[T <: Txn[T]] <: proc.Runner[T]

  /** Runs the process without custom arguments. */
  def run : Act = Runner.Run  (this)

  /** Stops the process. */
  def stop: Act = Runner.Stop (this)

  /** Runs the process with a list of given key-value arguments. */
  def runWith(attr: Ex[(String, _)]*): Act = Runner.RunWith(this, attr)

//  def runWith(attr: (String, Ex[_])*): Act = Runner.RunWith(this, attr)

  /** 0 - stopped, 1 - preparing, 2 - prepared, 3 - running, 4 - done, 5 - failed */
  def state: Ex[Int] = Runner.State(this)

  /** Triggers if the state becomes 0 */
  def stopped: Trig = isStopped.toTrig

  /** State is 0. */
  def isStopped: Ex[Boolean] = state sig_== 0

  /** Triggers if the state becomes 2 */
  def running: Trig = isRunning.toTrig

  /** State is 2. */
  def isRunning: Ex[Boolean] = state sig_== 2

  /** Triggers if the state becomes 4 */
  def done: Trig = isDone.toTrig

  /** State is 4. */
  def isDone: Ex[Boolean] = state sig_== 4

  /** Triggers if the state becomes 5 */
  def failed: Trig = hasFailed.toTrig

  /** State is 5. */
  def hasFailed: Ex[Boolean] = state sig_== 5

  /** Triggers if the state becomes 0 or 4 */
  def stoppedOrDone : Trig = isStoppedOrDone.toTrig

  /** State is 0 or 4. */
  def isStoppedOrDone: Ex[Boolean] = {
    val s = state
    (s sig_== 0) || (s sig_== 4)
  }

  /** Triggers if the state becomes 0, 4, or 5 */
  def idle: Trig = isIdle.toTrig

  /** State is 0, 4, or 5. Opposite of `busy` */
  def isIdle: Ex[Boolean] = {
    val s = state
    (s sig_== 0) || (s >= 4)  // stopped, or done, or failed
  }

  /** Triggers if the state becomes 1, 2, or 3. */
  def busy: Trig = isBusy.toTrig

  /** State is 1, 2, or 3. Opposite of `idle` */
  def isBusy: Ex[Boolean] = {
    val s = state
    (s >= 1) && (s <= 3)
  }

  /** Zero to one. Negative if unknown */
  def progress: Ex[Double] = Runner.Progress(this)

  /** A message tuple is (time, level, text) where level is 0 (info), 1 (warn) or 2 (error). */
  def messages: Ex[Seq[(Long, Int, String)]] = Runner.Messages(this)
}
