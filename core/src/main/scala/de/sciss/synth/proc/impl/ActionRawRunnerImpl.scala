///*
// *  ActionRawRunnerImpl.scala
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
//import de.sciss.synth.proc
//import de.sciss.synth.proc.Runner.{Attr, Done, Failed, Prepared, Running, Stopped}
//import de.sciss.synth.proc.{Action, ObjViewBase, Runner, TimeRef, Universe}
//
//import scala.concurrent.stm.Ref
//import scala.util.{Failure, Success, Try}
//
///** The action runner supports passing a value in by
//  * using the entry `"value"` in the `prepare` method's `attr` argument.
//  */
//object ActionRawRunnerImpl {
//  def apply[T <: Txn[T]](obj: ActionRaw[T])(implicit tx: T, universe: Universe[T]): Runner[T] =
//    new Impl(tx.newHandle(obj), universe)
//
//  abstract class Base[T <: Txn[T], Target] extends ObjViewBase[T, Target] with BasicViewBaseImpl[T] {
//    // ---- abstract ----
//
//    implicit def universe: proc.Universe[T]
//
//    type Repr = ActionRaw[T]
//
//    // ---- impl ----
//
//    final def tpe: Obj.Type = ActionRaw
//
//    final def prepare(timeRef: TimeRef.Option)(implicit tx: T): Unit = {
//      state = Prepared
//    }
//
//    final def stop()(implicit tx: T): Unit =
//      state = Stopped
//
////    def run(timeRef: TimeRef.Option, target: Target)(implicit tx: T): Unit =
//
//    final protected def execute(invokeValue: Any)(implicit tx: T): Unit = {
//      state = Running
//      val action = obj
//      state = if (action.muted) Stopped else {
//        val au = Action.Universe[T](action, value = invokeValue)
//        val tr = Try(action.execute(au))
//        tr match {
//          case Success(_)   => Done
//          case Failure(ex)  => Failed(ex)
//        }
//      }
//    }
//  }
//
//  private final class Impl[T <: Txn[T]](objH: stm.Source[T, ActionRaw[T]], override val universe: Universe[T])
//    extends Base[T, Unit] with BasicRunnerImpl[T] {
//
//    override def toString = s"Runner.Action${hashCode().toHexString}"
//
//    protected def disposeData()(implicit tx: T): Unit = ()
//
//    override def obj(implicit tx: T): ActionRaw[T] = objH()
//
//    private[this] val invokerValRef = Ref[Any](())
//
//    def prepare(attr: Attr[T])(implicit tx: T): Unit = {
//      invokerValRef() = attr.get("value").getOrElse(())
//      state = Prepared
//    }
//
//    def run()(implicit tx: T): Unit = {
//      val v = invokerValRef.swap(())
//      execute(v)
//    }
//
//    def run(timeRef: TimeRef.Option, target: Unit)(implicit tx: T): Unit =
//      execute(())
//
//    object progress extends Runner.Progress[T] with DummyObservableImpl[T] {
//      def current(implicit tx: T): Double = -1
//    }
//  }
//}
