/*
 *  Bounce.scala
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

package de.sciss.lucre.expr.graph

import java.awt.EventQueue

import de.sciss.file._
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{Context, IExpr}
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.{stm, synth}
import de.sciss.numbers
import de.sciss.equal.Implicits._
import de.sciss.processor.Processor
import de.sciss.span.Span
import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.impl.BasicRunnerImpl
import de.sciss.synth.proc.{SoundProcesses, Universe}
import de.sciss.synth.{Client, Server, proc}

import scala.concurrent.ExecutionContext
import scala.concurrent.stm.Ref
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object Bounce {
  // XXX TODO ugly
  private[sciss] var applyAudioPreferences: (Server.ConfigBuilder, Client.ConfigBuilder) => Unit = (_, _) => ()

  def apply(obj: Ex[Seq[Obj]], out: Ex[File], spec: Ex[AudioFileSpec], span: Ex[Span]): Bounce =
    Impl(obj, out, spec, span)

  private final class PeerImpl[S <: synth.Sys[S]](obj : IExpr[S, Seq[Obj]],
                                                  out : IExpr[S, File],
                                                  spec: IExpr[S, AudioFileSpec],
                                                  span: IExpr[S, Span],
                                                 )
                                                 (implicit val universe: Universe[S]) extends BasicRunnerImpl[S] {

    private[this] val procRef = Ref(Option.empty[Processor[File]])
    private[this] val procRun = Ref(0)

    object progress extends proc.Runner.Progress[S#Tx] with ObservableImpl[S, Double] {
      def current(implicit tx: S#Tx): Double =
        procRef().fold(0.0)(_.progress)

      def current_=(value: Double)(implicit tx: S#Tx): Unit = fire(value)
    }

    def prepare(attr: proc.Runner.Attr[S])(implicit tx: S#Tx): Unit = ()  // XXX TODO currently attr unused

    private def disposeProc()(implicit tx: S#Tx): Unit = {
      procRef.swap(None).foreach { p =>
        procRun.transform(_ + 1)
        tx.afterCommit {
          p.abort()
        }
      }
    }

    def run()(implicit tx: S#Tx): Unit = {
      disposeProc()

      val cfg                 = proc.Bounce.Config[S]()
      val bnc                 = proc.Bounce[S]()
      val specV               = spec.value
      val objV                = obj.value
      val objH                = objV.iterator.flatMap(_.peer[S].map(tx.newHandle(_: stm.Obj[S]))).toList
      cfg.group               = objH
      // Application.applyAudioPreferences(cfg.server, cfg.client, useDevice = false, pickPort = false)
//      applyAudioPreferences(cfg.server, cfg.client)
      val sCfg                = cfg.server
      sCfg.nrtOutputPath      = out.value.path // audioF.path
      sCfg.nrtHeaderFormat    = specV.fileType
      sCfg.nrtSampleFormat    = specV.sampleFormat
      val numPrivate          = 256 // Prefs.audioNumPrivate.getOrElse(Prefs.defaultAudioNumPrivate)
//      sCfg.sampleRate         = specV.sampleRate.toInt // sampleRate
      cfg.span                = span.value
      val procId              = procRun.transformAndGet(_ + 1)

      def atomic(name: String)(body: S#Tx => Unit): Unit =
        SoundProcesses.step[S](name) { implicit tx =>
          val _procRun = procRun()
          // println(s"step($name); ${_procRun} vs $procId")
          if (_procRun === procId) body(tx)
        }

      state = proc.Runner.Preparing

      tx.afterCommit {
        // XXX TODO ugly; we must access preferences on the EDT
        def start(): Unit = {
          try {
            // N.B.: several things are overwritten in `applyAudioPreferences`,
            // therefore be sure to set `sampleRate` etc. after this call
            applyAudioPreferences(cfg.server, cfg.client)
//            import SoundProcesses.executionContext
            import numbers.Implicits._
            sCfg.inputBusChannels   = 0
            sCfg.outputBusChannels  = 1
            sCfg.audioBusChannels   = (sCfg.outputBusChannels + numPrivate).nextPowerOfTwo
            sCfg.wireBuffers        = math.max(sCfg.wireBuffers, 1024) // possibly higher than default
            sCfg.sampleRate = specV.sampleRate.toInt // sampleRate
            val p           = bnc(cfg.build)
            p.addListener {
              case Processor.Result(_, tr) =>
                atomic("bounce.result") { implicit tx =>
                  procRef() = None
                  state = tr match {
                    case Success(_)   => proc.Runner.Done
                    case Failure(ex)  => proc.Runner.Failed(ex)
                  }
                }

              case Processor.Progress(_, amt) =>
                atomic("bounce.progress") { implicit tx =>
                  progress.current_=(amt)
                }
            }
            p.start()(ExecutionContext.global)  // N.B.: we'll starve if we use the single-threaded SP default here!
            // println("---1")
            atomic("bounce.running") { implicit tx =>
              // println("---2")
              procRef() = Some(p)
              state = proc.Runner.Running
            }
            // println("---3")
          } catch {
            case NonFatal(ex) =>
              // println("---4")
              atomic("bounce.failed") { implicit tx =>
                state = proc.Runner.Failed(ex)
              }
          }
        }

        if (EventQueue.isDispatchThread) start() else
          EventQueue.invokeLater { () =>
            start()
            // println("---7")
          }
      }
    }

    def stop()(implicit tx: S#Tx): Unit = {
      disposeProc()
      state = proc.Runner.Stopped
    }

    protected def disposeData()(implicit tx: S#Tx): Unit =
      disposeProc()
  }

  private final case class Impl(obj: Ex[Seq[Obj]], out: Ex[File], spec: Ex[AudioFileSpec], span: Ex[Span])
    extends Bounce {

    override def productPrefix: String = "Bounce" // serialization

    type Repr[S <: Sys[S]] = proc.Runner[S]

    protected def mkRepr[S <: Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] =
      tx.system match {
        case _: synth.Sys[_] =>
          // XXX TODO --- ugly ugly ugly
          mkControlImpl[synth.NoSys](ctx.asInstanceOf[Context[synth.NoSys]], tx.asInstanceOf[synth.NoSys#Tx])
            .asInstanceOf[Repr[S]]

        case _ => throw new Exception("Need a SoundProcesses system")
      }

    private def mkControlImpl[S <: synth.Sys[S]](implicit ctx: Context[S], tx: S#Tx): Repr[S] = {
      import ctx.{cursor, workspace}
      implicit val h: Universe[S] = Universe()
      new PeerImpl[S](obj.expand[S], out.expand[S], spec.expand[S], span.expand[S])
    }
  }
}
trait Bounce extends Runner {
  def obj : Ex[Seq[Obj]]
  def out : Ex[File]
  def spec: Ex[AudioFileSpec]
  def span: Ex[Span]
}
