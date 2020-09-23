/*
 *  AudioCue.scala
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

import de.sciss.file.File
import de.sciss.lucre
import de.sciss.lucre.Event.Targets
import de.sciss.lucre.{Artifact, Copy, DoubleObj, Elem, Expr, LongObj, Sys, Txn, expr, stm, Var => LVar}
import de.sciss.lucre.expr.graph.{Ex, AudioCue => _AudioCue}
import de.sciss.lucre.impl.ExprTypeImpl
import de.sciss.model.Change
import de.sciss.serial.{ConstFormat, DataInput, DataOutput}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc
import de.sciss.synth.proc.ExImport.audioFileSpecOps

import scala.annotation.switch

object AudioCue {
  final val typeId = 13

  def init(): Unit = Obj.init()

  private final val COOKIE = 0x4143 // 'AC'

  implicit object format extends ConstFormat[AudioCue] {
    def write(v: AudioCue, out: DataOutput): Unit = {
      import v._
      // out.writeByte(audioCookie)
      out.writeShort(AudioCue.COOKIE)
      out.writeUTF(artifact.getPath) // artifact.write(out)
      AudioFileSpec.Serializer.write(spec, out)
      out.writeLong(offset)
      out.writeDouble(gain)
    }

    def read(in: DataInput): AudioCue = {
      val cookie = in.readShort()
      if (cookie != COOKIE) sys.error(s"Unexpected cookie $cookie, expected $COOKIE")
      val artifact  = new File(in.readUTF())
      val spec      = AudioFileSpec.Serializer.read(in)
      val offset    = in.readLong()
      val gain      = in.readDouble()
      AudioCue(artifact = artifact, spec = spec, offset = offset, gain = gain)
    }
  }

  object Obj extends ExprTypeImpl[AudioCue, AudioCue.Obj] {
    def typeId: Int = AudioCue.typeId

    import AudioCue.{Obj => Repr}

    private[this] lazy val _init: Unit = {
      Obj     .registerExtension(Ext)
      LongObj .registerExtension(LongTuple1s)
    }

    override def init(): Unit = {
      super.init()
      _init
    }

    def tryParse(value: Any): Option[AudioCue] = value match {
      case x: AudioCue  => Some(x)
      case _            => None
    }

    protected def mkConst[T <: Txn[T]](id: Ident[T], value: A)(implicit tx: T): Const[T] =
      new _Const[T](id, value)

    protected def mkVar[T <: Txn[T]](targets: Targets[T], vr: LVar[T, E[T]], connect: Boolean)
                                    (implicit tx: T): Var[T] = {
      val res = new _Var[T](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[T <: Txn[T]](val id: Ident[T], val constValue: A)
      extends ConstImpl[T] with Repr[T] {

      def spec(implicit tx: T): AudioFileSpec = constValue.spec
    }

    private final class _Var[T <: Txn[T]](val targets: Targets[T], val ref: LVar[T, E[T]])
      extends VarImpl[T] with Repr[T] {

      def spec(implicit tx: T): AudioFileSpec = ref().spec
    }

    def valueSerializer: ConstFormat[AudioCue] = AudioCue.format

    def apply[T <: Txn[T]](artifact: Artifact[T], spec: AudioFileSpec, offset: LongObj[T], gain: DoubleObj[T])
                          (implicit tx: T): Obj[T] = {
      val targets = Targets[T]
      new Apply(targets, artifact = artifact, specValue = spec, offset = offset, gain = gain).connect()
    }

    def unapply[T <: Txn[T]](expr: Obj[T]): Option[(Artifact[T], AudioFileSpec, LongObj[T], DoubleObj[T])] =
      expr match {
        case impl: Apply[T] => Some((impl.artifact, impl.specValue, impl.offset, impl.gain))
        case _ => None
      }

    private object Ext extends Type.Extension1[Obj] {
      final val applyOpId         = 0
      final val replaceOffsetOpId = 1
      final val shiftOpId         = 2

      def readExtension[T <: Txn[T]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[T])
                                    (implicit tx: T): Obj[T] = {
        (opId: @switch) match {
          case `applyOpId` =>
            val artifact  = Artifact .read(in, access)
            val spec      = AudioFileSpec.Serializer.read(in)
            val offset    = LongObj  .read(in, access)
            val gain      = DoubleObj.read(in, access)
            new Apply(targets, artifact = artifact, specValue = spec, offset = offset, gain = gain)
          case `replaceOffsetOpId` =>
            val peer      = Obj      .read(in, access)
            val offset    = LongObj  .read(in, access)
            new ReplaceOffset(targets, peer = peer, offset = offset)
          case `shiftOpId` =>
            val peer      = Obj      .read(in, access)
            val amount    = LongObj  .read(in, access)
            new Shift(targets, peer = peer, amount = amount)
          case other =>
            sys.error(s"Unknown op-id $other")
        }
      }

      def name: String = "AudioCue Ops"

      val opLo: Int = applyOpId
      val opHi: Int = shiftOpId
    }
    final class Apply[T <: Txn[T]](protected val targets: Targets[T],
                                   val artifact: Artifact[T],
                                   val specValue: AudioFileSpec,
                                   val offset: LongObj[T],
                                   val gain: DoubleObj[T])
      extends lucre.expr.impl.NodeImpl[T, AudioCue] with Obj[T] {

      def tpe: stm.Obj.Type = AudioCue.Obj

      def spec(implicit tx: T): AudioFileSpec = specValue

      def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
        new Apply(Targets[Out], artifact = context(artifact), specValue = specValue, offset = context(offset),
          gain = context(gain)).connect()

      def value(implicit tx: T): AudioCue =
        AudioCue(artifact = artifact.value, spec = specValue, offset = offset.value, gain = gain.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[AudioCue]] = {
          val artifactEvt = artifact.changed
          val artifactChO = if (pull.contains(artifactEvt)) pull(artifactEvt) else None
          val offsetEvt   = offset.changed
          val offsetChO   = if (pull.contains(offsetEvt  )) pull(offsetEvt  ) else None
          val gainEvt     = gain.changed
          val gainChO     = if (pull.contains(gainEvt    )) pull(gainEvt    ) else None

          if (artifactChO.isEmpty && offsetChO.isEmpty && gainChO.isEmpty) return None

          val artifactCh = artifactChO.getOrElse {
            val artifactV = artifact.value
            Change(artifactV, artifactV)
          }

          val offsetCh = offsetChO.getOrElse {
            val offsetV = offset.value
            Change(offsetV, offsetV)
          }

          val gainCh = gainChO.getOrElse {
            val gainV = gain.value
            Change(gainV, gainV)
          }

          val before  = AudioCue(artifactCh.before, specValue, offset = offsetCh.before, gain = gainCh.before)
          val now     = AudioCue(artifactCh.now   , specValue, offset = offsetCh.now,    gain = gainCh.now   )

          Some(Change(before, now))
        }
      }

      protected def disposeData()(implicit tx: T): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(Ext.applyOpId)
        artifact .write(out)
        AudioFileSpec.Serializer.write(specValue, out)
        offset   .write(out)
        gain     .write(out)
      }

      def connect()(implicit tx: T): this.type = {
        artifact.changed ---> changed
        offset  .changed ---> changed
        gain    .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: T): Unit = {
        artifact.changed -/-> changed
        offset  .changed -/-> changed
        gain    .changed -/-> changed
      }
    }

    sealed trait LongOpImpl[T <: Txn[T]]
      extends lucre.expr.impl.NodeImpl[T, AudioCue] with Obj[T] {

      // ---- abstract ----

      def peer: E[T]

      protected def num: LongObj[T]

      protected def opId: Int

      protected def mapNum(peerValue: AudioCue, numValue: Long): AudioCue

      // ---- impl ----

      final def tpe: stm.Obj.Type = AudioCue.Obj

      final def spec (implicit tx: T): AudioFileSpec = peer.spec

      final def value(implicit tx: T): AudioCue = {
        val peerValue = peer.value
        val numValue  = num .value
        mapNum(peerValue, numValue)
      }

      object changed extends Changed {
        def pullUpdate(pull: Pull[T])(implicit tx: T): Option[Change[AudioCue]] = {
          val peerEvt     = peer.changed
          val peerChO     = if (pull.contains(peerEvt)) pull(peerEvt) else None
          val numEvt      = num.changed
          val numChO      = if (pull.contains(numEvt )) pull(numEvt ) else None

          if (peerChO.isEmpty && numChO.isEmpty) return None

          val peerCh = peerChO.getOrElse {
            val peerV = peer.value
            Change(peerV, peerV)
          }

          val numCh = numChO.getOrElse {
            val numV  = num.value
            Change(numV, numV)
          }

          val before  = mapNum(peerCh.before, numCh.before)
          val now     = mapNum(peerCh.now   , numCh.now   )

          Some(Change(before, now))
        }
      }

      final def connect()(implicit tx: T): this.type = {
        peer.changed ---> changed
        num .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: T): Unit = {
        peer.changed -/-> changed
        num .changed -/-> changed
      }

      protected final def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(opId)
        peer.write(out)
        num .write(out)
      }

      protected def disposeData()(implicit tx: T): Unit = disconnect()
    }

    object ReplaceOffset {
      def unapply[T <: Txn[T]](ex: Obj[T]): Option[(Obj[T], LongObj[T])] = ex match {
        case s: ReplaceOffset[T] => Some((s.peer, s.offset))
        case _ => None
      }
      def apply[T <: Txn[T]](peer: E[T], offset: LongObj[T])(implicit tx: T): ReplaceOffset[T] = {
        new ReplaceOffset(Targets[T], peer, offset).connect()
      }
    }
    final class ReplaceOffset[T <: Txn[T]](protected val targets: Targets[T],
                                           val peer: Obj[T],
                                           val offset: LongObj[T])
      extends LongOpImpl[T] {

      protected def num: LongObj[T] = offset

      protected def opId: Int = Ext.replaceOffsetOpId

      protected def mapNum(peerValue: AudioCue, numValue: Long): AudioCue =
        peerValue.copy(offset = numValue)

      def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
        new ReplaceOffset(Targets[Out], peer = context(peer), offset = context(offset)).connect()
    }

    object Shift {
      def unapply[T <: Txn[T]](ex: Obj[T]): Option[(Obj[T], LongObj[T])] = ex match {
        case s: Shift[T] => Some((s.peer, s.amount))
        case _ => None
      }
      def apply[T <: Txn[T]](peer: Obj[T], amount: LongObj[T])(implicit tx: T): Shift[T] = {
        new Shift(Targets[T], peer, amount).connect()
      }
    }
    final class Shift[T <: Txn[T]](protected val targets: Targets[T],
                                   val peer: Obj[T],
                                   val amount: LongObj[T])
      extends LongOpImpl[T] {

      protected def num: LongObj[T] = amount

      protected def opId: Int = Ext.shiftOpId

      protected def mapNum(peerValue: AudioCue, numValue: Long): AudioCue =
        if (numValue == 0L) peerValue else peerValue.copy(offset = peerValue.offset + numValue)

      def copy[Out <: Sys[Out]]()(implicit tx: T, txOut: Out#Tx, context: Copy[T, Out]): Elem[Out] =
        new Shift(Targets[Out], peer = context(peer), amount = context(amount)).connect()
    }

    final class Ops[T <: Txn[T]](val `this`: E[T]) extends AnyVal { me =>
      import me.{`this` => ex}

      def replaceOffset(newValue: LongObj[T])(implicit tx: T): E[T] = (ex, newValue) match {
        case (a: Apply[T], _) => Obj(artifact = a.artifact, spec = a.specValue, offset = newValue, gain = a.gain)
        case (Epr.Const(c), Epr.Const(offset)) => newConst(c.copy(offset = offset))
        case _ =>
          new ReplaceOffset(Targets[T], peer = ex, offset = newValue).connect()
      }

      def offset(implicit tx: T): LongObj[T] = ex match {
        case a: Apply[T]    => a.offset
        case Epr.Const(c) => LongObj.newConst[T](c.offset)
        case _              => Offset[T](ex)
      }

      def shift(amount: LongObj[T])(implicit tx: T): E[T] = (ex, amount) match {
        case (Epr.Const(c), Epr.Const(amountC)) => newConst(c.copy(offset = c.offset + amountC))
        case (s: Shift[T], _) =>
          import proc.Ops.longObjOps
//          s.amount match {
//            case LongObj.Var(amtVr) =>
//              amtVr() = amtVr() + amount
//              ex
//            case _ =>
              new Shift(Targets[T], peer = s.peer, amount = s.amount + amount).connect()
//          }
        case _ =>
          new Shift(Targets[T], peer = ex, amount = amount).connect()
      }
    }

    private[this] object LongTuple1s extends Type.Extension1[LongObj] {
      // final val arity = 1
      final val opLo: Int = Offset.id
      final val opHi: Int = Offset.id

      val name = "AudioCue-Long Ops"

      def readExtension[T <: Txn[T]](opId: Int, in: DataInput, targets: Targets[T])
                                    (implicit tx: T): LongObj[T] = {
        val op: LongOp = opId match {
          case Offset.id => Offset
        }
        op.read(in, targets)
      }
    }

    sealed abstract class LongOp extends LongExtensions.UnaryOp.Op[AudioCue, Obj] {
      final def read[T <: Txn[T]](in: DataInput, targets: Targets[T])
                                 (implicit tx: T): LongObj[T] = {
        val _1 = Obj.read(in)
        new LongExtensions.Tuple1[T, AudioCue, Obj](targets, this, _1)
      }
    }

    case object Offset extends LongOp {
      final val id = 30
      def value(a: AudioCue): Long = a.offset
    }
  }
  sealed trait Obj[T <: Txn[T]] extends Expr[T, AudioCue] {
    def spec(implicit tx: T): AudioFileSpec

    /** A simple forward to `spec.numChannels` */
    def numChannels(implicit tx: T): Int = spec.numChannels

    /** A simple forward to `spec.numFrames` */
    def numFrames(implicit tx: T): Long = spec.numFrames

    /** A simple forward to `spec.sampleRate` */
    def sampleRate(implicit tx: T): Double = spec.sampleRate
  }

  implicit final class ExOps(private val x: Ex[AudioCue]) extends AnyVal {
    def artifact: Ex[File]          = _AudioCue.Artifact(x)
    def spec    : Ex[AudioFileSpec] = _AudioCue.Spec    (x)
    def offset  : Ex[Long]          = _AudioCue.Offset  (x)
    def gain    : Ex[Double]        = _AudioCue.Gain    (x)

    /** A simple forward to `spec.numChannels` */
    def numChannels : Ex[Int]     = spec.numChannels

    /** A simple forward to `spec.numFrames` */
    def numFrames   : Ex[Long]    = spec.numFrames

    /** A simple forward to `spec.sampleRate` */
    def sampleRate  : Ex[Double]  = spec.sampleRate

    /** A utility method that reports the offset with respect to the file's sample rate.
      * That is, it multiplies `offset` by the factor `this.sampleRate / TimeRef.SampleRate`
      */
    def fileOffset: Ex[Long] = _AudioCue.FileOffset(x) // (offset / TimeRef.SampleRate * sampleRate + 0.5).toLong
  }

  implicit object ExValue extends Ex.Value[AudioCue]
}

/** A chunk of an audio file.
  *
  * @param artifact   the audio file
  * @param spec       the audio file spec, carrying information about duration, sample rate, number of channels
  * @param offset     an offset into the file, ''using `TimeRef.SampleRate` as its base''
  * @param gain       a linear gain factor
  */
final case class AudioCue(artifact: Artifact.Value, spec: AudioFileSpec, offset: Long, gain: Double) {
  /** A simple forward to `spec.numChannels` */
  def numChannels: Int = spec.numChannels

  /** A simple forward to `spec.numFrames` */
  def numFrames: Long = spec.numFrames

  /** A simple forward to `spec.sampleRate` */
  def sampleRate: Double = spec.sampleRate

  /** A utility method that reports the offset with respect to the file's sample rate.
    * That is, it multiplies `offset` by the factor `this.sampleRate / TimeRef.SampleRate`
    */
  def fileOffset: Long = (offset / TimeRef.SampleRate * sampleRate + 0.5).toLong
}