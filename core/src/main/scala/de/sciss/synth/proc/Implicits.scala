/*
 *  Implicits.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2017 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import de.sciss.lucre.expr.{BooleanObj, Expr, StringObj}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.span.SpanLike

import scala.language.implicitConversions

object Implicits {
  implicit class SecFrames(val `this`: Double) extends AnyVal { me =>
    import me.{`this` => d}

    /** Interprets the number as a duration in seconds, and converts it to sample frames,
      * based on the standard `Timeline` sample-rate.
      */
    def secondsToFrames: Long = (d * TimeRef.SampleRate + 0.5).toLong
  }
  
  implicit class SpanComparisons(val `this`: SpanLike) extends AnyVal {
    import `this`.{compareStart, compareStop}

    def startsBefore(frame: Long): Boolean = compareStart(frame) <  0
    def startsAt    (frame: Long): Boolean = compareStart(frame) == 0
    def startsAfter (frame: Long): Boolean = compareStart(frame) >  0

    def stopsBefore (frame: Long): Boolean = compareStop (frame) <  0
    def stopsAt     (frame: Long): Boolean = compareStop (frame) == 0
    def stopsAfter  (frame: Long): Boolean = compareStop (frame) >  0
  }

// SCAN
//  implicit class ScanOps[S <: Sys[S]](val `this`: Scan[S]) extends AnyVal {
//    /** Connects this scan as source to that scan as sink. */
//    def ~> (that: Scan[S])(implicit tx: S#Tx): Unit =
//      `this`.add(Scan.Link.Scan(that))
//
//    /** Disconnects this scan as source from that scan as sink. */
//    def ~/> (that: Scan[S])(implicit tx: S#Tx): Unit =
//      `this`.remove(Scan.Link.Scan(that))
//  }

// SCAN
//  // Scala 2.10.4 has a compiler bug that prevents putting this
//  // code inside a value class
//  private def getScanLinks[S <: Sys[S]](in: Iterator[Scan.Link[S]])
//                                             (implicit tx: S#Tx): Set[Scan.Link.Scan[S]] =
//    in.collect {
//      case l @ Scan.Link.Scan(_) => l
//    } .toSet

//  implicit class ProcPairOps[S <: Sys[S]](val `this`: (Proc[S], Proc[S])) extends AnyVal { me =>
//    import me.{`this` => pair}
//
// SCAN
//    private def getLayerIn(implicit tx: S#Tx): Scan[S] = {
//      val inObj = pair._1
//      inObj.inputs.get("in").getOrElse(sys.error(s"Proc ${inObj.name} does not have scan 'in'"))
//    }

// SCAN
//    private def getLayerOut(implicit tx: S#Tx): Scan[S] = {
//      val outObj = pair._2
//      outObj.outputs.get("out").getOrElse(sys.error(s"Proc ${outObj.name} does not have scan 'out'"))
//    }

// SCAN
//    /** Removes the signal chain signified by the input proc pair from its predecessors and successors.
//      * It does so by removing the sources of the `_1` scan named `"in"` and the sinks of the
//      * `_2` scan named `"out"`. It re-connects the predecessors and successors thus found.
//      */
//    def unlink()(implicit tx: S#Tx): Unit = {
//      val layerIn   = getLayerIn
//      val layerOut  = getLayerOut
//
//      val oldLayerIn  = getScanLinks(layerIn .iterator)
//      val oldLayerOut = getScanLinks(layerOut.iterator)
//
//      // disconnect old inputs
//      oldLayerIn .foreach(layerIn .remove)
//      // disconnect old outputs
//      oldLayerOut.foreach(layerOut.remove)
//      // connect old layer inputs to old layer outputs
//      oldLayerIn.foreach { in =>
//        oldLayerOut.foreach { out =>
//          in.peer.add(out)
//        }
//      }
//    }

// SCAN
//    def linkAfter(out: Proc[S])(implicit tx: S#Tx): Unit = {
//      val target = out.outputs.get("out").getOrElse(sys.error(s"Successor ${out.name} does not have scan 'out'"))
//      link1(target, isAfter = true)
//    }

// SCAN
//    def linkBefore(in: Proc[S])(implicit tx: S#Tx): Unit = {
//      val target = in.inputs.get("in").getOrElse(sys.error(s"Predecessor ${in.name} does not have scan 'in'"))
//      link1(target, isAfter = false)
//    }

// SCAN
//    private def link1(target: Scan[S], isAfter: Boolean)(implicit tx: S#Tx): Unit = {
//      val layerIn  = getLayerIn
//      val layerOut = getLayerOut
//
//      val targetIt  = target.iterator
//      val oldTargetLinks = getScanLinks(targetIt)
//      val layerLink = Scan.Link.Scan(if (isAfter) layerIn else layerOut)
//      // only act if we're not there
//      if (!oldTargetLinks.contains(layerLink)) {
//        unlink()
//        if (isAfter) {
//          // disconnect old diff outputs
//          oldTargetLinks.foreach(target.remove)
//          // connect old diff inputs as new layer inputs
//          oldTargetLinks.foreach(layerOut.add)
//          // connect layer output to diff input
//          target.add(layerLink)
//        } else {
//          // disconnect old diff inputs
//          oldTargetLinks.foreach(target.remove)
//          // connect old diff inputs as new layer inputs
//          oldTargetLinks.foreach(layerIn.add  )
//          // connect layer output to diff input
//          target.add(layerLink)
//        }
//      }
//    }
//  }

  implicit class FolderOps[S <: Sys[S]](val `this`: Folder[S]) extends AnyVal { me =>
    import me.{`this` => folder}

    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = folder.iterator.filter { obj =>
        obj.name == child
      } .toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in $folder")
      res
    }
  }

  implicit class EnsembleOps[S <: Sys[S]](val `this`: Ensemble[S]) extends AnyVal { me =>
    import me.{`this` => ensemble}

    def / (child: String)(implicit tx: S#Tx): Option[Obj[S]] = {
      val res = ensemble.folder.iterator.filter { obj =>
        obj.name == child
      }.toList.headOption

      // if (res.isEmpty) warn(s"Child $child not found in ${ensemble.attr.name}")
      res
    }

    def play()(implicit tx: S#Tx): Unit = play1(value = true )
    def stop()(implicit tx: S#Tx): Unit = play1(value = false)

    private def play1(value: Boolean)(implicit tx: S#Tx): Unit = {
      val BooleanObj.Var(vr) = ensemble.playing
      val prev = vr()
      if (!(Expr.isConst(prev) && prev.value == value)) vr() = value
    }

    def isPlaying(implicit tx: S#Tx): Boolean = ensemble.playing.value
  }

  implicit final class ObjOps[S <: Sys[S]](val `this`: Obj[S]) extends AnyVal { me =>
    import me.{`this` => obj}

    /** Short cut for accessing the attribute `"name"`.
      * If their is no value found, a dummy string `"&lt;unnamed&gt;"` is returned.
      */
    def name(implicit tx: S#Tx): String =
      obj.attr.$[StringObj](ObjKeys.attrName).fold("<unnamed>")(_.value)

    /** Short cut for updating the attribute `"name"`. */
    def name_=(value: String)(implicit tx: S#Tx): Unit = {
      val valueC  = StringObj.newConst[S](value)
      val attr    = obj.attr
      attr.$[StringObj](ObjKeys.attrName) match {
        case Some(StringObj.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = StringObj.newVar(valueC)
          attr.put(ObjKeys.attrName, valueVr)
      }
    }

    /** Short cut for accessing the attribute `"mute"`. */
    def muted(implicit tx: S#Tx): Boolean =
      obj.attr.$[BooleanObj](ObjKeys.attrMute).exists(_.value)

    /** Short cut for updating the attribute `"mute"`. */
    def muted_=(value: Boolean)(implicit tx: S#Tx): Unit = {
      val valueC  = BooleanObj.newConst[S](value)
      val attr    = obj.attr
      attr.$[BooleanObj](ObjKeys.attrMute) match {
        case Some(BooleanObj.Var(vr)) => vr() = valueC
        case _                  =>
          val valueVr = BooleanObj.newVar(valueC)
          attr.put(ObjKeys.attrMute, valueVr)
      }
    }
  }
}