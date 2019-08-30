/*
 *  TimeRef.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2019 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Affero General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.span.{SpanLike, Span}

object TimeRef {
  /** `Undefined` with general type */
  def undefined: Option = Undefined

  final val SampleRate = 14112000.0 // lcm(88.2k, 96k)

  /** Utility method that generates a string representation of
    * the time in seconds of a given frame index.
    */
  def framesToSecs(n: Long): String = if (n == Long.MinValue) "-inf" else if (n == Long.MaxValue) "inf" else {
    val s = n / SampleRate
    f"$s%1.3fs"
  }

  /** Utility method that generates a string representation of
    * the time both in frames and in seconds.
    */
  def framesAndSecs(n: Long): String = {
    val frames = if (n == Long.MinValue) "-inf" else if (n == Long.MaxValue) "inf" else n.toString
    s"$frames / ${framesToSecs(n)}"
  }

  def spanToSecs(span: SpanLike): String = span match {
    case Span.Void => "(void)"
    case Span.All => "(all)"
    case s: Span.Bounded =>
      val start = s match {
        case hs: Span.HasStart => framesToSecs(hs.start)
        case _ => "-inf"
      }
      val stop = s match {
        case hs: Span.HasStop => framesToSecs(hs.stop)
        case _ => "inf"
      }
      s"($start - $stop)"
  }

  def spanAndSecs(span: SpanLike): String = s"$span / ${spanToSecs(span)}"

  private val FromZero = new TimeRef(Span.From(0L), offset = 0L)

  case object Undefined extends Option {
    /** For convenience, an undefined time references reports a frame of zero. */
    val frame           = 0L
    /** The span is of an undefined time reference is void. */
    val span: SpanLike  = Span.Void
    val offset          = 0L
    val isDefined       = false

    def hasEnded        = true

    def force: TimeRef  = FromZero
  }

  /** A time reference specifies the temporal context
    * within which an aural object is invoked. It may
    * be either undefined (there is no notion of
    * pointers in virtual performance time), or
    * defined by a span for the invoked object and
    * a time frame corresponding to the current position.
    */
  sealed trait Option {
    /** The overall played span. */
    def span : SpanLike

    /** `true` for `TimeRef`, `false` for `TimeRef.Undefined`. */
    def isDefined: Boolean

    /** `true` for a `TimeRef` whose `offset` is smaller than its span start, `false` for `TimeRef.Undefined`. */
    def hasEnded: Boolean

    /** Offsets within the logical span of the object.
      * For an undefined time reference, this will report zero,
      * otherwise it is equal to `frame - span.start`. If the
      * span does not have a defined start, it will also report zero.
      */
    def offset: Long

    /** If the reference is undefined, translates it into a defined one beginning at zero
      * with a span from zero. Otherwise returns the reference unmodified.
      */
    def force: TimeRef
  }
}

final case class TimeRef(span: Span.HasStart, offset: Long) extends TimeRef.Option {

  def frame : Long = offset + span.start

  def isDefined       = true
  def force: TimeRef  = this

  def shift       (deltaFrames: Long): TimeRef = if (deltaFrames == 0   ) this else copy(offset = offset + deltaFrames)
  def updateOffset(newOffset  : Long): TimeRef = if (offset == newOffset) this else copy(offset = newOffset           )

  def child(that: SpanLike): TimeRef.Option =
    that match {
      case s: Span.HasStart => new TimeRef(s, offset = offset - s.start)
      case _  => // Until or Void or All
        val spanZ = span.shift(-span.start)   // move it to "zero"
        val span1 = spanZ.intersect(that)     // enforce a start of zero or void
        span1 match {
          case s: Span.HasStart =>
            assert(s.start == 0L)
            new TimeRef(s, offset = offset)
          case _ => TimeRef.Undefined  // was void
        }
    }

  def hasEnded: Boolean = span.compareStop(offset) <= 0

  import TimeRef.{spanAndSecs, framesAndSecs}

  override def toString = s"TimeRef(span = ${spanAndSecs(span)}, frame = ${framesAndSecs(frame)})"
}