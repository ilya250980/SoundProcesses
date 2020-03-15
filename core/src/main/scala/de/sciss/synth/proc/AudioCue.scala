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
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.event.{Pull, Targets}
import de.sciss.lucre.expr.graph.{Ex, AudioCue => _AudioCue}
import de.sciss.lucre.expr.{DoubleObj, LongExtensions, LongObj, Type, Expr => _Expr}
import de.sciss.lucre.stm.{Copy, Elem, Sys}
import de.sciss.lucre.{expr, stm}
import de.sciss.model.Change
import de.sciss.serial.{DataInput, DataOutput, ImmutableSerializer}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc
import de.sciss.synth.proc.ExImport.audioFileSpecOps

import scala.annotation.switch

object AudioCue {
  final val typeId = 13

  def init(): Unit = Obj.init()

  private final val COOKIE = 0x4143 // 'AC'

  implicit object serializer extends ImmutableSerializer[AudioCue] {
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

  object Obj extends expr.impl.ExprTypeImpl[AudioCue, AudioCue.Obj] {
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

    protected def mkConst[S <: Sys[S]](id: S#Id, value: A)(implicit tx: S#Tx): Const[S] =
      new _Const[S](id, value)

    protected def mkVar[S <: Sys[S]](targets: Targets[S], vr: S#Var[_Ex[S]], connect: Boolean)
                                    (implicit tx: S#Tx): Var[S] = {
      val res = new _Var[S](targets, vr)
      if (connect) res.connect()
      res
    }

    private final class _Const[S <: Sys[S]](val id: S#Id, val constValue: A)
      extends ConstImpl[S] with Repr[S] {

      def spec(implicit tx: S#Tx): AudioFileSpec = constValue.spec
    }

    private final class _Var[S <: Sys[S]](val targets: Targets[S], val ref: S#Var[_Ex[S]])
      extends VarImpl[S] with Repr[S] {

      def spec(implicit tx: S#Tx): AudioFileSpec = ref().spec
    }

    def valueSerializer: ImmutableSerializer[AudioCue] = AudioCue.serializer

    def apply[S <: Sys[S]](artifact: Artifact[S], spec: AudioFileSpec, offset: LongObj[S], gain: DoubleObj[S])
                          (implicit tx: S#Tx): Obj[S] = {
      val targets = Targets[S]
      new Apply(targets, artifact = artifact, specValue = spec, offset = offset, gain = gain).connect()
    }

    def unapply[S <: Sys[S]](expr: Obj[S]): Option[(Artifact[S], AudioFileSpec, LongObj[S], DoubleObj[S])] =
      expr match {
        case impl: Apply[S] => Some((impl.artifact, impl.specValue, impl.offset, impl.gain))
        case _ => None
      }

    private object Ext extends Type.Extension1[Obj] {
      final val applyOpId         = 0
      final val replaceOffsetOpId = 1
      final val shiftOpId         = 2

      def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                    (implicit tx: S#Tx): Obj[S] = {
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
    final class Apply[S <: Sys[S]](protected val targets: Targets[S],
                                   val artifact: Artifact[S],
                                   val specValue: AudioFileSpec,
                                   val offset: LongObj[S],
                                   val gain: DoubleObj[S])
      extends lucre.expr.impl.NodeImpl[S, AudioCue] with Obj[S] {

      def tpe: stm.Obj.Type = AudioCue.Obj

      def spec(implicit tx: S#Tx): AudioFileSpec = specValue

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Apply(Targets[Out], artifact = context(artifact), specValue = specValue, offset = context(offset),
          gain = context(gain)).connect()

      def value(implicit tx: S#Tx): AudioCue =
        AudioCue(artifact = artifact.value, spec = specValue, offset = offset.value, gain = gain.value)

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change[AudioCue]] = {
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

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()

      protected def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(Ext.applyOpId)
        artifact .write(out)
        AudioFileSpec.Serializer.write(specValue, out)
        offset   .write(out)
        gain     .write(out)
      }

      def connect()(implicit tx: S#Tx): this.type = {
        artifact.changed ---> changed
        offset  .changed ---> changed
        gain    .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: S#Tx): Unit = {
        artifact.changed -/-> changed
        offset  .changed -/-> changed
        gain    .changed -/-> changed
      }
    }

    sealed trait LongOpImpl[S <: Sys[S]]
      extends lucre.expr.impl.NodeImpl[S, AudioCue] with Obj[S] {

      // ---- abstract ----

      def peer: _Ex[S]

      protected def num: LongObj[S]

      protected def opId: Int

      protected def mapNum(peerValue: AudioCue, numValue: Long): AudioCue

      // ---- impl ----

      final def tpe: stm.Obj.Type = AudioCue.Obj

      final def spec (implicit tx: S#Tx): AudioFileSpec = peer.spec

      final def value(implicit tx: S#Tx): AudioCue = {
        val peerValue = peer.value
        val numValue  = num .value
        mapNum(peerValue, numValue)
      }

      object changed extends Changed {
        def pullUpdate(pull: Pull[S])(implicit tx: S#Tx): Option[Change[AudioCue]] = {
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

      final def connect()(implicit tx: S#Tx): this.type = {
        peer.changed ---> changed
        num .changed ---> changed
        this
      }

      private def disconnect()(implicit tx: S#Tx): Unit = {
        peer.changed -/-> changed
        num .changed -/-> changed
      }

      protected final def writeData(out: DataOutput): Unit = {
        out.writeByte(1)  // 'node' not 'var'
        out.writeInt(opId)
        peer.write(out)
        num .write(out)
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = disconnect()
    }

    object ReplaceOffset {
      def unapply[S <: Sys[S]](ex: Obj[S]): Option[(Obj[S], LongObj[S])] = ex match {
        case s: ReplaceOffset[S] => Some((s.peer, s.offset))
        case _ => None
      }
      def apply[S <: Sys[S]](peer: _Ex[S], offset: LongObj[S])(implicit tx: S#Tx): ReplaceOffset[S] = {
        new ReplaceOffset(Targets[S], peer, offset).connect()
      }
    }
    final class ReplaceOffset[S <: Sys[S]](protected val targets: Targets[S],
                                           val peer: Obj[S],
                                           val offset: LongObj[S])
      extends LongOpImpl[S] {

      protected def num: LongObj[S] = offset

      protected def opId: Int = Ext.replaceOffsetOpId

      protected def mapNum(peerValue: AudioCue, numValue: Long): AudioCue =
        peerValue.copy(offset = numValue)

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new ReplaceOffset(Targets[Out], peer = context(peer), offset = context(offset)).connect()
    }

    object Shift {
      def unapply[S <: Sys[S]](ex: Obj[S]): Option[(Obj[S], LongObj[S])] = ex match {
        case s: Shift[S] => Some((s.peer, s.amount))
        case _ => None
      }
      def apply[S <: Sys[S]](peer: Obj[S], amount: LongObj[S])(implicit tx: S#Tx): Shift[S] = {
        new Shift(Targets[S], peer, amount).connect()
      }
    }
    final class Shift[S <: Sys[S]](protected val targets: Targets[S],
                                   val peer: Obj[S],
                                   val amount: LongObj[S])
      extends LongOpImpl[S] {

      protected def num: LongObj[S] = amount

      protected def opId: Int = Ext.shiftOpId

      protected def mapNum(peerValue: AudioCue, numValue: Long): AudioCue =
        if (numValue == 0L) peerValue else peerValue.copy(offset = peerValue.offset + numValue)

      def copy[Out <: Sys[Out]]()(implicit tx: S#Tx, txOut: Out#Tx, context: Copy[S, Out]): Elem[Out] =
        new Shift(Targets[Out], peer = context(peer), amount = context(amount)).connect()
    }

    final class Ops[S <: Sys[S]](val `this`: _Ex[S]) extends AnyVal { me =>
      import me.{`this` => ex}

      def replaceOffset(newValue: LongObj[S])(implicit tx: S#Tx): _Ex[S] = (ex, newValue) match {
        case (a: Apply[S], _) => Obj(artifact = a.artifact, spec = a.specValue, offset = newValue, gain = a.gain)
        case (_Expr.Const(c), _Expr.Const(offset)) => newConst(c.copy(offset = offset))
        case _ =>
          new ReplaceOffset(Targets[S], peer = ex, offset = newValue).connect()
      }

      def offset(implicit tx: S#Tx): LongObj[S] = ex match {
        case a: Apply[S]    => a.offset
        case _Expr.Const(c) => LongObj.newConst[S](c.offset)
        case _              => Offset[S](ex)
      }

      def shift(amount: LongObj[S])(implicit tx: S#Tx): _Ex[S] = (ex, amount) match {
        case (_Expr.Const(c), _Expr.Const(amountC)) => newConst(c.copy(offset = c.offset + amountC))
        case (s: Shift[S], _) =>
          import proc.Ops.longObjOps
//          s.amount match {
//            case LongObj.Var(amtVr) =>
//              amtVr() = amtVr() + amount
//              ex
//            case _ =>
              new Shift(Targets[S], peer = s.peer, amount = s.amount + amount).connect()
//          }
        case _ =>
          new Shift(Targets[S], peer = ex, amount = amount).connect()
      }
    }

    private[this] object LongTuple1s extends Type.Extension1[LongObj] {
      // final val arity = 1
      final val opLo: Int = Offset.id
      final val opHi: Int = Offset.id

      val name = "AudioCue-Long Ops"

      def readExtension[S <: Sys[S]](opId: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                    (implicit tx: S#Tx): LongObj[S] = {
        val op: LongOp = opId match {
          case Offset.id => Offset
        }
        op.read(in, access, targets)
      }
    }

    sealed abstract class LongOp extends LongExtensions.UnaryOp.Op[AudioCue, Obj] {
      final def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: Targets[S])
                                 (implicit tx: S#Tx): LongObj[S] = {
        val _1 = Obj.read(in, access)
        new LongExtensions.Tuple1[S, AudioCue, Obj](targets, this, _1)
      }
    }

    case object Offset extends LongOp {
      final val id = 30
      def value(a: AudioCue): Long = a.offset
    }
  }
  sealed trait Obj[S <: Sys[S]] extends _Expr[S, AudioCue] {
    def spec(implicit tx: S#Tx): AudioFileSpec

    /** A simple forward to `spec.numChannels` */
    def numChannels(implicit tx: S#Tx): Int = spec.numChannels

    /** A simple forward to `spec.numFrames` */
    def numFrames(implicit tx: S#Tx): Long = spec.numFrames

    /** A simple forward to `spec.sampleRate` */
    def sampleRate(implicit tx: S#Tx): Double = spec.sampleRate
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