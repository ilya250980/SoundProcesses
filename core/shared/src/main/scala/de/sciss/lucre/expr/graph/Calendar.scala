/*
 *  Calendar.scala
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

import de.sciss.lucre.Txn.peer
import de.sciss.lucre.expr.ExElem.{ProductReader, RefMapIn}
import de.sciss.lucre.expr.impl.IActionImpl
import de.sciss.lucre.expr.{Context, IAction, IControl, ITrigger}
import de.sciss.lucre.impl.IGeneratorEvent
import de.sciss.lucre.{IEvent, IExpr, IPull, ITargets, Txn}
import de.sciss.proc.{ExprContext, Scheduler, TimeRef}

import java.time.temporal.{ChronoField, ChronoUnit, TemporalField, TemporalUnit}
import java.time.{DateTimeException, Instant, ZoneId, ZonedDateTime => _Calendar}
import scala.concurrent.stm.TSet
import scala.util.control.NonFatal

object Calendar extends ProductReader[Ex[Calendar]] {
  def apply(stamp: Ex[Long] = TimeStamp(), zone: Ex[String] = "default"): Ex[Calendar] = Apply(stamp, zone)

  override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Ex[Calendar] = {
    require (arity == 2 && adj == 0)
    val _stamp  = in.readEx[Long]()
    val _zone   = in.readEx[String]()
    Calendar(_stamp, _zone)
  }

  /** Unit of milliseconds, or the field of milliseconds of second (0 to 999) */
  final val Milli  = 0
  /** Unit of seconds, or the field of seconds of minute (0 to 59) */
  final val Second = 1
  /** Unit of minutes, or the field of minutes of hour (0 to 59) */
  final val Minute = 2
  /** Unit of hours, or the field of hour of day (0 to 23) */
  final val Hour   = 3
  /** Unit of days, or the field of day of month (starting at 1) */
  final val Day    = 4
  /** Unit of weeks, or the field of aligned week of year (1 to 53) */
  final val Week   = 5
  /** Unit of months, or the field of month of year (1 to 12) */
  final val Month  = 6
  /** Unit of years, or the field of year */
  final val Year   = 7

  /** Field of day of week (1 to 7, where 1 is Monday) */
  final val DayOfWeek   = 8

  private def mkUnit(i: Int): TemporalUnit = i match {
    case Milli  => ChronoUnit.MILLIS
    case Second => ChronoUnit.SECONDS
    case Minute => ChronoUnit.MINUTES
    case Hour   => ChronoUnit.HOURS
    case Day    => ChronoUnit.DAYS
    case Week   => ChronoUnit.WEEKS
    case Month  => ChronoUnit.MONTHS
    case Year   => ChronoUnit.YEARS
    case _      =>
      System.err.println(s"Illegal calendar unit $i")
      null
  }

  private def mkField(i: Int): TemporalField = i match {
    case Milli      => ChronoField.MILLI_OF_SECOND
    case Second     => ChronoField.SECOND_OF_MINUTE
    case Minute     => ChronoField.MINUTE_OF_HOUR
    case Hour       => ChronoField.HOUR_OF_DAY
    case Day        => ChronoField.DAY_OF_MONTH
    case Week       => ChronoField.ALIGNED_WEEK_OF_YEAR
    case Month      => ChronoField.MONTH_OF_YEAR
    case Year       => ChronoField.YEAR
    case DayOfWeek  => ChronoField.DAY_OF_WEEK
//    case WeekOfMonth  => ChronoField.ALIGNED_WEEK_OF_MONTH
    case _          =>
      System.err.println(s"Illegal calendar field $i")
      null
  }

  // only used in expansion, no serialization needed
  private case object TruncOp extends BinaryOp.Op[Calendar, Int, Calendar] {
    override def apply(in: Calendar, unitI: Int): Calendar = {
      val unit = mkUnit(unitI)
      if (unit == null) in else try {
        new Wrap(in.peer.truncatedTo(unit))
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          in
      }
    }
  }

  // only used in expansion, no serialization needed
  private case object SetOp extends TernaryOp.Op[Calendar, Int, Int, Calendar] {
    override def apply(in: Calendar, unitI: Int, value: Int): Calendar = {
      val field = mkField(unitI)
      if (field == null) in else try {
        new Wrap(in.peer.`with`(field, value.toLong))
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          in
      }
    }
  }

  // only used in expansion, no serialization needed
  private case object GetOp extends BinaryOp.Op[Calendar, Int, Int] {
    override def apply(in: Calendar, fieldI: Int): Int = {
      val field = mkField(fieldI)
      if (field == null) 0 else try {
        in.peer.get(field)
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          0
      }
    }
  }

  private final class Wrap(val peer: _Calendar) extends Calendar with Proxy {
    def self: Any = peer
  }

  // only used in expansion, no serialization needed
  private case object AddOp extends TernaryOp.Op[Calendar, Int, Int, Calendar] {
    override def apply(in: Calendar, unitI: Int, value: Int): Calendar = {
      val unit = mkUnit(unitI)
      if (unit == null) in else try {
        new Wrap(in.peer.plus(value.toLong, unit))
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          in
      }
    }
  }

  object Trunc extends ProductReader[Trunc] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Trunc = {
      require (arity == 2 && adj == 0)
      val _in   = in.readEx[Calendar]()
      val _unit = in.readEx[Int]()
      new Trunc(_in, _unit)
    }
  }
  final case class Trunc(in: Ex[Calendar], unit: Ex[Int]) extends Ex[Calendar] {
    override def productPrefix: String = s"Calendar$$Trunc" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Calendar]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new BinaryOp.Expanded(TruncOp, in.expand[T], unit.expand[T], tx)
    }
  }

  object Set extends ProductReader[Set] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Set = {
      require (arity == 3 && adj == 0)
      val _in     = in.readEx[Calendar]()
      val _field  = in.readEx[Int]()
      val _value  = in.readEx[Int]()
      new Set(_in, _field, _value)
    }
  }
  final case class Set(in: Ex[Calendar], field: Ex[Int], value: Ex[Int]) extends Ex[Calendar] {
    override def productPrefix: String = s"Calendar$$Set" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Calendar]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new TernaryOp.Expanded(SetOp, in.expand[T], field.expand[T], value.expand[T], tx)
    }
  }

  object Add extends ProductReader[Add] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Add = {
      require (arity == 3 && adj == 0)
      val _in     = in.readEx[Calendar]()
      val _unit   = in.readEx[Int]()
      val _value  = in.readEx[Int]()
      new Add(_in, _unit, _value)
    }
  }
  final case class Add(in: Ex[Calendar], unit: Ex[Int], value: Ex[Int]) extends Ex[Calendar] {
    override def productPrefix: String = s"Calendar$$Add" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Calendar]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new TernaryOp.Expanded(AddOp, in.expand[T], unit.expand[T], value.expand[T], tx)
    }
  }

  object Get extends ProductReader[Get] {
    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Get = {
      require (arity == 2 && adj == 0)
      val _in     = in.readEx[Calendar]()
      val _field  = in.readEx[Int]()
      new Get(_in, _field)
    }
  }
  final case class Get(in: Ex[Calendar], field: Ex[Int]) extends Ex[Int] {
    override def productPrefix: String = s"Calendar$$Get" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Int]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new BinaryOp.Expanded(GetOp, in.expand[T], field.expand[T], tx)
    }
  }

  private case object ApplyOp extends BinaryOp.Op[Long, String, Calendar] {
    override def apply(stamp: Long, zoneS: String): Calendar = {
      val inst = try {
        Instant.ofEpochMilli(stamp)
      } catch {
        case ex: DateTimeException =>
          ex.printStackTrace()
          val secs = Math.floorDiv(stamp, 1000)
          if (secs < Instant.MIN.getEpochSecond) Instant.MIN else Instant.MAX
      }
      val peer = try {
        val zone = if (zoneS == "default") ZoneId.systemDefault() else ZoneId.of(zoneS)
        inst.atZone(zone)
      } catch {
        case NonFatal(ex) =>
          ex.printStackTrace()
          _Calendar.now()
      }
      new Wrap(peer)
    }
  }

  private final class ExpandedSchedule[T <: Txn[T]](in: IExpr[T, Calendar])
                                                   (implicit protected val targets: ITargets[T], scheduler: Scheduler[T])
    extends Schedule.Repr[T] with IGeneratorEvent[T, Unit] with IActionImpl[T] {

    private[this] val tokens = TSet.empty[Int]

    def cancel()(implicit tx: T): Unit = {
      tokens.foreach(scheduler.cancel)
      tokens.clear()
    }

    def executeAction()(implicit tx: T): Unit = {
      val inV     = in.value.peer
      val t1      = TimeStamp.ref()
      val t2      = inV.toInstant.toEpochMilli
      if (t1 > t2) return

      val dt      = /*if (t2 > t1)*/ t2 - t1 /*else 0L*/
      val frames  = (dt * (TimeRef.SampleRate / 1000.0)).toLong
      lazy val token: Int = scheduler.schedule(scheduler.time + frames) { implicit tx =>
        tokens.remove(token)
        fire(())
      }
      tokens.add(token)
      ()
    }

    private[lucre] def pullUpdate(pull: IPull[T])(implicit tx: T): Option[Unit] =
      Trig.Some

    override def dispose()(implicit tx: T): Unit = {
      super.dispose()
      cancel()
    }

    def initControl()(implicit tx: T): Unit = ()

    def changed: IEvent[T, Unit] = this
  }

  object Schedule extends ProductReader[Schedule] {
    trait Repr[T <: Txn[T]] extends IControl[T] with IAction[T] with ITrigger[T] {
      def cancel()(implicit tx: T): Unit
    }

    override def read(in: RefMapIn, key: String, arity: Int, adj: Int): Schedule = {
      require (arity == 1 && adj == 0)
      val _in = in.readEx[Calendar]()
      new Schedule(_in)
    }
  }

  final case class Schedule(in: Ex[Calendar]) extends Act with Trig {
    override def productPrefix: String = s"Calendar$$Schedule" // serialization

    type Repr[T <: Txn[T]] = Schedule.Repr[T]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      val ec = ExprContext.get
      val u = ec.universe
      import ctx.targets
      import u.scheduler
      new ExpandedSchedule(in.expand[T])
    }
  }

  private final case class Apply(stamp: Ex[Long], zone: Ex[String]) extends Ex[Calendar] {
    override def productPrefix: String = "Calendar" // serialization

    type Repr[T <: Txn[T]] = IExpr[T, Calendar]

    protected def mkRepr[T <: Txn[T]](implicit ctx: Context[T], tx: T): Repr[T] = {
      import ctx.targets
      new BinaryOp.Expanded(ApplyOp, stamp.expand[T], zone.expand[T], tx)
    }
  }

  implicit final class Ops(private val in: Ex[Calendar]) extends AnyVal {
    def trunc(unit: Ex[Int]): Ex[Calendar] = Trunc(in, unit)

    def set(field : Ex[Int], value: Ex[Int]): Ex[Calendar] = Set(in, field , value)
    def add(unit  : Ex[Int], value: Ex[Int]): Ex[Calendar] = Add(in, unit  , value)

    def get(field : Ex[Int]): Ex[Int] = Get(in, field)

    /** Schedules a trigger for the current calendar when the action is invoked.
      * Different from `Delay`, if the calendar lies in the past, the event is '''not''' scheduled.
      * Furthermore, each action invocation schedules a new event instead of cancelling the previous event.
      * Calling `cancel` on the `Schedule` will cancel all scheduled events.
      */
    def schedule: Schedule = Schedule(in)
  }
}
trait Calendar {
  private[lucre] def peer: _Calendar
}